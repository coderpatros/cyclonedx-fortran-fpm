! This file is part of CycloneDX SBOM generator for Fortran fpm projects
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
! http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
! SPDX-License-Identifier: Apache-2.0
! Copyright (c) Patrick Dwyer. All Rights Reserved.

module cyclonedx_generator
    use json_module, only: json_core, json_value
    use dependency_resolver, only: dep_graph_t, dep_node_t
    use manifest_parser, only: package_t, dependency_t, SOURCE_GIT, SOURCE_PATH, SOURCE_REGISTRY
    use uuid_module, only: generate_uuid4
    use iso8601_module, only: get_iso8601_timestamp
    use string_utils, only: extract_github_owner_repo
    implicit none
    private

    public :: generate_sbom

    character(len=*), parameter :: TOOL_NAME = 'cyclonedx-fpm'
    character(len=*), parameter :: TOOL_VERSION = '0.1.0'
    character(len=*), parameter :: SPEC_VERSION = '1.6'

contains

    !> Generate CycloneDX 1.6 JSON SBOM from a resolved dependency graph
    subroutine generate_sbom(graph, json_output, ierr)
        type(dep_graph_t), intent(in) :: graph
        character(len=:), allocatable, intent(out) :: json_output
        integer, intent(out) :: ierr

        type(json_core) :: json
        type(json_value), pointer :: root, metadata, tools, tool_arr, tool_obj
        type(json_value), pointer :: meta_component, components, comp_obj
        type(json_value), pointer :: dep_array, dep_obj, depends_on
        type(json_value), pointer :: ext_refs, ext_ref_obj, licenses, lic_obj, lic_inner
        character(len=36) :: uuid
        character(len=20) :: timestamp
        character(len=:), allocatable :: purl, version_str, scope_str
        character(len=:), allocatable :: owner, repo
        logical :: is_github
        integer :: i, j

        ierr = 0

        call json%initialize(compress_vectors=.true.)

        ! Create root object
        call json%create_object(root, '')

        ! Top-level fields
        call json%add(root, 'bomFormat', 'CycloneDX')
        call json%add(root, 'specVersion', SPEC_VERSION)
        call json%add(root, 'version', 1)

        call generate_uuid4(uuid)
        call json%add(root, 'serialNumber', 'urn:uuid:' // uuid)

        ! Metadata
        call json%create_object(metadata, 'metadata')
        call json%add(root, metadata)

        call get_iso8601_timestamp(timestamp)
        call json%add(metadata, 'timestamp', trim(timestamp))

        ! metadata.tools.components[]
        call json%create_object(tools, 'tools')
        call json%add(metadata, tools)
        call json%create_array(tool_arr, 'components')
        call json%add(tools, tool_arr)
        call json%create_object(tool_obj, '')
        call json%add(tool_arr, tool_obj)
        call json%add(tool_obj, 'type', 'application')
        call json%add(tool_obj, 'name', TOOL_NAME)
        call json%add(tool_obj, 'version', TOOL_VERSION)

        ! metadata.component (root project)
        call json%create_object(meta_component, 'component')
        call json%add(metadata, meta_component)
        call add_component_fields(json, meta_component, graph%nodes(graph%root_index), &
                                   is_root=.true.)

        ! components[] array
        call json%create_array(components, 'components')
        call json%add(root, components)

        do i = 1, graph%node_count
            if (i == graph%root_index) cycle

            call json%create_object(comp_obj, '')
            call json%add(components, comp_obj)
            call add_component_fields(json, comp_obj, graph%nodes(i), is_root=.false.)
        end do

        ! dependencies[] array
        call json%create_array(dep_array, 'dependencies')
        call json%add(root, dep_array)

        do i = 1, graph%node_count
            call json%create_object(dep_obj, '')
            call json%add(dep_array, dep_obj)
            call json%add(dep_obj, 'ref', trim(graph%nodes(i)%bom_ref))

            call json%create_array(depends_on, 'dependsOn')
            call json%add(dep_obj, depends_on)

            do j = 1, graph%nodes(i)%child_count
                call json%add(depends_on, '', &
                    trim(graph%nodes(graph%nodes(i)%child_indices(j))%bom_ref))
            end do
        end do

        ! Serialize to string
        call json%serialize(root, json_output)
        call json%destroy(root)
    end subroutine generate_sbom

    !> Add component fields to a JSON object
    subroutine add_component_fields(json, obj, node, is_root)
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: obj
        type(dep_node_t), intent(in) :: node
        logical, intent(in) :: is_root

        type(json_value), pointer :: ext_refs, ext_ref_obj, licenses, lic_obj, lic_inner
        character(len=:), allocatable :: purl, version_str, scope_str
        character(len=:), allocatable :: owner, repo
        logical :: is_github

        if (is_root) then
            call json%add(obj, 'type', 'application')
        else
            call json%add(obj, 'type', 'library')
        end if

        call json%add(obj, 'bom-ref', trim(node%bom_ref))
        call json%add(obj, 'name', trim(node%package%name))

        ! Version
        version_str = get_version(node)
        if (len_trim(version_str) > 0) then
            call json%add(obj, 'version', trim(version_str))
        end if

        ! Description
        if (len_trim(node%package%description) > 0) then
            call json%add(obj, 'description', trim(node%package%description))
        end if

        ! Author
        if (len_trim(node%package%author) > 0) then
            call json%add(obj, 'author', trim(node%package%author))
        end if

        ! Scope (required/optional for dev deps)
        if (.not. is_root) then
            if (node%is_dev) then
                call json%add(obj, 'scope', 'optional')
            else
                call json%add(obj, 'scope', 'required')
            end if
        end if

        ! PURL
        purl = build_purl(node)
        if (len_trim(purl) > 0) then
            call json%add(obj, 'purl', purl)
        end if

        ! Licenses
        if (len_trim(node%package%license) > 0) then
            call json%create_array(licenses, 'licenses')
            call json%add(obj, licenses)
            call json%create_object(lic_obj, '')
            call json%add(licenses, lic_obj)
            call json%create_object(lic_inner, 'license')
            call json%add(lic_obj, lic_inner)
            call json%add(lic_inner, 'id', trim(node%package%license))
        end if

        ! External references
        call add_external_refs(json, obj, node)
    end subroutine add_component_fields

    !> Build a PURL string for a dependency
    function build_purl(node) result(purl)
        type(dep_node_t), intent(in) :: node
        character(len=:), allocatable :: purl
        character(len=:), allocatable :: owner, repo, version_str
        logical :: is_github

        purl = ''
        version_str = get_version(node)

        if (node%source_dep%source_kind == SOURCE_GIT) then
            call extract_github_owner_repo(trim(node%source_dep%git_url), owner, repo, is_github)
            if (is_github .and. len_trim(owner) > 0 .and. len_trim(repo) > 0) then
                purl = 'pkg:github/' // owner // '/' // repo
                if (len_trim(version_str) > 0) then
                    purl = purl // '@' // trim(version_str)
                end if
            else
                purl = 'pkg:generic/' // trim(node%package%name)
                if (len_trim(version_str) > 0) then
                    purl = purl // '@' // trim(version_str)
                end if
            end if
        else if (len_trim(node%package%name) > 0) then
            purl = 'pkg:generic/' // trim(node%package%name)
            if (len_trim(version_str) > 0) then
                purl = purl // '@' // trim(version_str)
            end if
        end if
    end function build_purl

    !> Get the version string for a node (from package or dependency info)
    function get_version(node) result(ver)
        type(dep_node_t), intent(in) :: node
        character(len=:), allocatable :: ver

        ver = ''
        if (len_trim(node%package%version) > 0) then
            ver = trim(node%package%version)
        else if (len_trim(node%source_dep%git_tag) > 0) then
            ver = trim(node%source_dep%git_tag)
        else if (len_trim(node%source_dep%git_rev) > 0) then
            ver = trim(node%source_dep%git_rev)
        else if (len_trim(node%source_dep%git_branch) > 0) then
            ver = trim(node%source_dep%git_branch)
        else if (len_trim(node%source_dep%version_spec) > 0) then
            ver = trim(node%source_dep%version_spec)
        end if
    end function get_version

    !> Add external references (VCS, homepage) to a component
    subroutine add_external_refs(json, obj, node)
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: obj
        type(dep_node_t), intent(in) :: node

        type(json_value), pointer :: ext_refs, ref_obj
        logical :: has_refs

        has_refs = .false.

        if (len_trim(node%source_dep%git_url) > 0 .or. &
            len_trim(node%package%homepage) > 0) then
            has_refs = .true.
        end if

        if (.not. has_refs) return

        call json%create_array(ext_refs, 'externalReferences')
        call json%add(obj, ext_refs)

        if (len_trim(node%source_dep%git_url) > 0) then
            call json%create_object(ref_obj, '')
            call json%add(ext_refs, ref_obj)
            call json%add(ref_obj, 'type', 'vcs')
            call json%add(ref_obj, 'url', trim(node%source_dep%git_url))
        end if

        if (len_trim(node%package%homepage) > 0) then
            call json%create_object(ref_obj, '')
            call json%add(ext_refs, ref_obj)
            call json%add(ref_obj, 'type', 'website')
            call json%add(ref_obj, 'url', trim(node%package%homepage))
        end if
    end subroutine add_external_refs

end module cyclonedx_generator
