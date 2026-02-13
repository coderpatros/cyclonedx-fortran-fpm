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

module dependency_resolver
    use manifest_parser, only: dependency_t, package_t, parse_manifest_file, &
                                SOURCE_GIT, SOURCE_PATH, SOURCE_REGISTRY
    use command_module, only: git_clone, make_temp_dir, remove_dir
    use string_utils, only: join_path
    use uuid_module, only: generate_uuid4
    implicit none
    private

    public :: dep_node_t, dep_graph_t, resolve_dependencies

    integer, parameter, public :: MAX_PACKAGES = 256
    integer, parameter :: MAX_CHILDREN = 64

    type :: dep_node_t
        type(package_t) :: package
        type(dependency_t) :: source_dep
        character(len=64) :: bom_ref = ''
        logical :: is_dev = .false.
        integer :: child_indices(MAX_CHILDREN) = 0
        integer :: child_count = 0
    end type dep_node_t

    type :: dep_graph_t
        type(dep_node_t), allocatable :: nodes(:)
        integer :: node_count = 0
        integer :: root_index = 1
    end type dep_graph_t

    ! Module-level temp directories to clean up
    character(len=512) :: temp_dirs(MAX_PACKAGES) = ''
    integer :: temp_dir_count = 0

contains

    !> Resolve all dependencies starting from a project root
    subroutine resolve_dependencies(project_path, include_dev, graph, ierr)
        character(len=*), intent(in) :: project_path
        logical, intent(in) :: include_dev
        type(dep_graph_t), intent(out) :: graph
        integer, intent(out) :: ierr

        character(len=:), allocatable :: manifest_path
        type(package_t) :: root_pkg
        character(len=36) :: uuid

        ierr = 0
        temp_dir_count = 0

        ! Allocate nodes on the heap
        allocate(graph%nodes(MAX_PACKAGES))

        ! Parse root manifest
        manifest_path = join_path(project_path, 'fpm.toml')
        call parse_manifest_file(manifest_path, root_pkg, ierr)
        if (ierr /= 0) return

        ! Add root as first node
        graph%node_count = 1
        graph%root_index = 1
        graph%nodes(1)%package = root_pkg
        call generate_uuid4(uuid)
        graph%nodes(1)%bom_ref = uuid

        ! Resolve regular dependencies
        call resolve_deps_recursive(project_path, root_pkg%dependencies, &
                                     root_pkg%dep_count, 1, .false., graph, ierr)
        if (ierr /= 0) goto 999

        ! Optionally resolve dev dependencies (first level only, then recurse into their regular deps)
        if (include_dev) then
            call resolve_dev_deps(project_path, root_pkg%dev_dependencies, &
                                   root_pkg%dev_dep_count, 1, graph, ierr)
            if (ierr /= 0) goto 999
        end if

999     continue
        ! Clean up temp directories
        call cleanup_temp_dirs()
    end subroutine resolve_dependencies

    !> Recursively resolve dependencies
    recursive subroutine resolve_deps_recursive(base_path, deps, dep_count, &
                                                  parent_idx, is_dev, graph, ierr)
        character(len=*), intent(in) :: base_path
        type(dependency_t), intent(in) :: deps(:)
        integer, intent(in) :: dep_count, parent_idx
        logical, intent(in) :: is_dev
        type(dep_graph_t), intent(inout) :: graph
        integer, intent(out) :: ierr

        integer :: i, existing_idx, new_idx
        character(len=:), allocatable :: clone_dir, dep_path
        type(package_t) :: dep_pkg

        ierr = 0

        do i = 1, dep_count
            ! Check if already visited (cycle detection)
            existing_idx = find_by_name(graph, trim(deps(i)%name))
            if (existing_idx > 0) then
                ! Just add edge, don't recurse
                call add_child(graph%nodes(parent_idx), existing_idx)
                cycle
            end if

            if (graph%node_count >= MAX_PACKAGES) then
                ierr = -1
                return
            end if

            select case (deps(i)%source_kind)
            case (SOURCE_GIT)
                call make_temp_dir('cdx_fpm', clone_dir, ierr)
                if (ierr /= 0) return

                temp_dir_count = temp_dir_count + 1
                temp_dirs(temp_dir_count) = clone_dir

                call git_clone(trim(deps(i)%git_url), clone_dir, &
                               tag=trim(deps(i)%git_tag), &
                               branch=trim(deps(i)%git_branch), &
                               rev=trim(deps(i)%git_rev), &
                               ierr=ierr)
                if (ierr /= 0) return

                call parse_manifest_file(join_path(clone_dir, 'fpm.toml'), dep_pkg, ierr)
                if (ierr /= 0) then
                    ierr = 0
                    dep_pkg = package_t()
                    dep_pkg%name = deps(i)%name
                    allocate(dep_pkg%dependencies(0))
                    allocate(dep_pkg%dev_dependencies(0))
                end if

                if (len_trim(dep_pkg%name) == 0) then
                    dep_pkg%name = deps(i)%name
                end if

                new_idx = add_node(graph, dep_pkg, deps(i), is_dev)
                call add_child(graph%nodes(parent_idx), new_idx)

                if (allocated(dep_pkg%dependencies)) then
                    call resolve_deps_recursive(clone_dir, dep_pkg%dependencies, &
                                                 dep_pkg%dep_count, new_idx, .false., graph, ierr)
                    if (ierr /= 0) return
                end if

            case (SOURCE_PATH)
                if (deps(i)%path(1:1) == '/') then
                    dep_path = trim(deps(i)%path)
                else
                    dep_path = join_path(base_path, trim(deps(i)%path))
                end if

                call parse_manifest_file(join_path(dep_path, 'fpm.toml'), dep_pkg, ierr)
                if (ierr /= 0) then
                    ierr = 0
                    dep_pkg = package_t()
                    dep_pkg%name = deps(i)%name
                    allocate(dep_pkg%dependencies(0))
                    allocate(dep_pkg%dev_dependencies(0))
                end if

                if (len_trim(dep_pkg%name) == 0) then
                    dep_pkg%name = deps(i)%name
                end if

                new_idx = add_node(graph, dep_pkg, deps(i), is_dev)
                call add_child(graph%nodes(parent_idx), new_idx)

                if (allocated(dep_pkg%dependencies)) then
                    call resolve_deps_recursive(dep_path, dep_pkg%dependencies, &
                                                 dep_pkg%dep_count, new_idx, .false., graph, ierr)
                    if (ierr /= 0) return
                end if

            case (SOURCE_REGISTRY)
                dep_pkg = package_t()
                dep_pkg%name = deps(i)%name
                dep_pkg%version = deps(i)%version_spec
                allocate(dep_pkg%dependencies(0))
                allocate(dep_pkg%dev_dependencies(0))

                new_idx = add_node(graph, dep_pkg, deps(i), is_dev)
                call add_child(graph%nodes(parent_idx), new_idx)
            end select
        end do
    end subroutine resolve_deps_recursive

    !> Resolve dev dependencies (first level only, then recurse into their regular deps)
    subroutine resolve_dev_deps(base_path, deps, dep_count, parent_idx, graph, ierr)
        character(len=*), intent(in) :: base_path
        type(dependency_t), intent(in) :: deps(:)
        integer, intent(in) :: dep_count, parent_idx
        type(dep_graph_t), intent(inout) :: graph
        integer, intent(out) :: ierr

        integer :: i, existing_idx, new_idx
        character(len=:), allocatable :: clone_dir, dep_path
        type(package_t) :: dep_pkg

        ierr = 0

        do i = 1, dep_count
            existing_idx = find_by_name(graph, trim(deps(i)%name))
            if (existing_idx > 0) then
                call add_child(graph%nodes(parent_idx), existing_idx)
                cycle
            end if

            if (graph%node_count >= MAX_PACKAGES) then
                ierr = -1
                return
            end if

            select case (deps(i)%source_kind)
            case (SOURCE_GIT)
                call make_temp_dir('cdx_fpm', clone_dir, ierr)
                if (ierr /= 0) return

                temp_dir_count = temp_dir_count + 1
                temp_dirs(temp_dir_count) = clone_dir

                call git_clone(trim(deps(i)%git_url), clone_dir, &
                               tag=trim(deps(i)%git_tag), &
                               branch=trim(deps(i)%git_branch), &
                               rev=trim(deps(i)%git_rev), &
                               ierr=ierr)
                if (ierr /= 0) return

                call parse_manifest_file(join_path(clone_dir, 'fpm.toml'), dep_pkg, ierr)
                if (ierr /= 0) then
                    ierr = 0
                    dep_pkg = package_t()
                    dep_pkg%name = deps(i)%name
                    allocate(dep_pkg%dependencies(0))
                    allocate(dep_pkg%dev_dependencies(0))
                end if

                if (len_trim(dep_pkg%name) == 0) then
                    dep_pkg%name = deps(i)%name
                end if

                new_idx = add_node(graph, dep_pkg, deps(i), .true.)
                call add_child(graph%nodes(parent_idx), new_idx)

                if (allocated(dep_pkg%dependencies)) then
                    call resolve_deps_recursive(clone_dir, dep_pkg%dependencies, &
                                                 dep_pkg%dep_count, new_idx, .false., graph, ierr)
                    if (ierr /= 0) return
                end if

            case (SOURCE_PATH)
                if (deps(i)%path(1:1) == '/') then
                    dep_path = trim(deps(i)%path)
                else
                    dep_path = join_path(base_path, trim(deps(i)%path))
                end if

                call parse_manifest_file(join_path(dep_path, 'fpm.toml'), dep_pkg, ierr)
                if (ierr /= 0) then
                    ierr = 0
                    dep_pkg = package_t()
                    dep_pkg%name = deps(i)%name
                    allocate(dep_pkg%dependencies(0))
                    allocate(dep_pkg%dev_dependencies(0))
                end if

                if (len_trim(dep_pkg%name) == 0) then
                    dep_pkg%name = deps(i)%name
                end if

                new_idx = add_node(graph, dep_pkg, deps(i), .true.)
                call add_child(graph%nodes(parent_idx), new_idx)

                if (allocated(dep_pkg%dependencies)) then
                    call resolve_deps_recursive(dep_path, dep_pkg%dependencies, &
                                                 dep_pkg%dep_count, new_idx, .false., graph, ierr)
                    if (ierr /= 0) return
                end if

            case (SOURCE_REGISTRY)
                dep_pkg = package_t()
                dep_pkg%name = deps(i)%name
                dep_pkg%version = deps(i)%version_spec
                allocate(dep_pkg%dependencies(0))
                allocate(dep_pkg%dev_dependencies(0))

                new_idx = add_node(graph, dep_pkg, deps(i), .true.)
                call add_child(graph%nodes(parent_idx), new_idx)
            end select
        end do
    end subroutine resolve_dev_deps

    !> Find a node by package name, returns index or 0 if not found
    function find_by_name(graph, name) result(idx)
        type(dep_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: name
        integer :: idx, i

        idx = 0
        do i = 1, graph%node_count
            if (trim(graph%nodes(i)%package%name) == trim(name)) then
                idx = i
                return
            end if
        end do
    end function find_by_name

    !> Add a new node to the graph, returns its index
    function add_node(graph, pkg, dep, is_dev) result(idx)
        type(dep_graph_t), intent(inout) :: graph
        type(package_t), intent(in) :: pkg
        type(dependency_t), intent(in) :: dep
        logical, intent(in) :: is_dev
        integer :: idx
        character(len=36) :: uuid

        graph%node_count = graph%node_count + 1
        idx = graph%node_count

        graph%nodes(idx)%package = pkg
        graph%nodes(idx)%source_dep = dep
        graph%nodes(idx)%is_dev = is_dev

        call generate_uuid4(uuid)
        graph%nodes(idx)%bom_ref = uuid
    end function add_node

    !> Add a child index to a node
    subroutine add_child(node, child_idx)
        type(dep_node_t), intent(inout) :: node
        integer, intent(in) :: child_idx
        integer :: i

        ! Check if already present
        do i = 1, node%child_count
            if (node%child_indices(i) == child_idx) return
        end do

        if (node%child_count < MAX_CHILDREN) then
            node%child_count = node%child_count + 1
            node%child_indices(node%child_count) = child_idx
        end if
    end subroutine add_child

    !> Clean up all temporary directories
    subroutine cleanup_temp_dirs()
        integer :: i, ierr

        do i = 1, temp_dir_count
            if (len_trim(temp_dirs(i)) > 0) then
                call remove_dir(trim(temp_dirs(i)), ierr)
                temp_dirs(i) = ''
            end if
        end do
        temp_dir_count = 0
    end subroutine cleanup_temp_dirs

end module dependency_resolver
