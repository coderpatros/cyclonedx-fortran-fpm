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

program test_generator
    use json_module, only: json_core, json_value, json_file
    use dependency_resolver, only: dep_graph_t, dep_node_t, MAX_PACKAGES
    use manifest_parser, only: package_t, dependency_t, SOURCE_GIT, SOURCE_PATH
    use cyclonedx_generator, only: generate_sbom
    implicit none

    integer :: failures

    failures = 0

    call test_sbom_top_level()
    call test_sbom_metadata_component()
    call test_sbom_components()
    call test_sbom_dependencies()
    call test_sbom_purls()

    if (failures > 0) then
        write(0, '(a,i0,a)') 'FAILED: ', failures, ' test(s) failed'
        error stop 1
    else
        write(*, '(a)') 'All generator tests passed.'
    end if

contains

    subroutine assert_true(test_name, cond)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: cond
        if (cond) then
            write(*, '(a,a)') 'PASS: ', test_name
        else
            write(*, '(a,a)') 'FAIL: ', test_name
            failures = failures + 1
        end if
    end subroutine

    subroutine assert_equals_str(test_name, expected, actual)
        character(len=*), intent(in) :: test_name, expected, actual
        if (trim(expected) == trim(actual)) then
            write(*, '(a,a)') 'PASS: ', test_name
        else
            write(*, '(a,a)') 'FAIL: ', test_name
            write(*, '(a,a,a)') '  expected: "', trim(expected), '"'
            write(*, '(a,a,a)') '  actual:   "', trim(actual), '"'
            failures = failures + 1
        end if
    end subroutine

    subroutine assert_equals_int(test_name, expected, actual)
        character(len=*), intent(in) :: test_name
        integer, intent(in) :: expected, actual
        if (expected == actual) then
            write(*, '(a,a)') 'PASS: ', test_name
        else
            write(*, '(a,a)') 'FAIL: ', test_name
            write(*, '(a,i0)') '  expected: ', expected
            write(*, '(a,i0)') '  actual:   ', actual
            failures = failures + 1
        end if
    end subroutine

    !> Build a mock dependency graph:
    !! Node 1: root "my-app" (application)
    !! Node 2: "dep-github" (GitHub git dependency)
    !! Node 3: "dep-generic" (non-GitHub git dependency)
    !! Root depends on both children.
    subroutine build_mock_graph(graph)
        type(dep_graph_t), intent(out) :: graph

        allocate(graph%nodes(MAX_PACKAGES))
        graph%node_count = 3
        graph%root_index = 1

        ! Node 1: root
        graph%nodes(1)%bom_ref = 'ref-root-00000000-0000-0000-0000'
        graph%nodes(1)%package%name = 'my-app'
        graph%nodes(1)%package%version = '1.0.0'
        graph%nodes(1)%package%license = 'Apache-2.0'
        graph%nodes(1)%package%description = 'Test app'
        graph%nodes(1)%is_dev = .false.
        graph%nodes(1)%child_count = 2
        graph%nodes(1)%child_indices(1) = 2
        graph%nodes(1)%child_indices(2) = 3
        allocate(graph%nodes(1)%package%dependencies(0))
        allocate(graph%nodes(1)%package%dev_dependencies(0))

        ! Node 2: GitHub git dep
        graph%nodes(2)%bom_ref = 'ref-github-0000-0000-0000-0000'
        graph%nodes(2)%package%name = 'dep-github'
        graph%nodes(2)%package%version = '2.0.0'
        graph%nodes(2)%package%license = 'MIT'
        graph%nodes(2)%is_dev = .false.
        graph%nodes(2)%source_dep%source_kind = SOURCE_GIT
        graph%nodes(2)%source_dep%git_url = 'https://github.com/owner/repo'
        graph%nodes(2)%child_count = 0
        allocate(graph%nodes(2)%package%dependencies(0))
        allocate(graph%nodes(2)%package%dev_dependencies(0))

        ! Node 3: non-GitHub git dep (generic purl)
        graph%nodes(3)%bom_ref = 'ref-generic-000-0000-0000-0000'
        graph%nodes(3)%package%name = 'dep-generic'
        graph%nodes(3)%package%version = '3.0.0'
        graph%nodes(3)%is_dev = .false.
        graph%nodes(3)%source_dep%source_kind = SOURCE_GIT
        graph%nodes(3)%source_dep%git_url = 'https://gitlab.com/other/lib'
        graph%nodes(3)%child_count = 0
        allocate(graph%nodes(3)%package%dependencies(0))
        allocate(graph%nodes(3)%package%dev_dependencies(0))
    end subroutine

    ! ---- Top-level fields ----

    subroutine test_sbom_top_level()
        type(dep_graph_t) :: graph
        character(len=:), allocatable :: json_output
        type(json_file) :: jf
        character(len=:), allocatable :: str_val
        integer :: int_val
        logical :: found

        call build_mock_graph(graph)
        call generate_sbom(graph, json_output, failures)

        call jf%initialize()
        call jf%deserialize(json_output)

        call jf%get('bomFormat', str_val, found)
        call assert_true('top-level: bomFormat found', found)
        call assert_equals_str('top-level: bomFormat', 'CycloneDX', str_val)

        call jf%get('specVersion', str_val, found)
        call assert_true('top-level: specVersion found', found)
        call assert_equals_str('top-level: specVersion', '1.6', str_val)

        call jf%get('version', int_val, found)
        call assert_true('top-level: version found', found)
        call assert_equals_int('top-level: version', 1, int_val)

        call jf%get('serialNumber', str_val, found)
        call assert_true('top-level: serialNumber found', found)
        call assert_true('top-level: serialNumber starts with urn:uuid:', &
            str_val(1:9) == 'urn:uuid:')

        call jf%destroy()
        deallocate(graph%nodes)
    end subroutine

    ! ---- metadata.component ----

    subroutine test_sbom_metadata_component()
        type(dep_graph_t) :: graph
        character(len=:), allocatable :: json_output
        type(json_file) :: jf
        character(len=:), allocatable :: str_val
        logical :: found
        integer :: ierr

        call build_mock_graph(graph)
        call generate_sbom(graph, json_output, ierr)

        call jf%initialize()
        call jf%deserialize(json_output)

        call jf%get('metadata.component.name', str_val, found)
        call assert_true('metadata.component: name found', found)
        call assert_equals_str('metadata.component: name', 'my-app', str_val)

        call jf%get('metadata.component.type', str_val, found)
        call assert_true('metadata.component: type found', found)
        call assert_equals_str('metadata.component: type', 'application', str_val)

        call jf%get('metadata.component.version', str_val, found)
        call assert_true('metadata.component: version found', found)
        call assert_equals_str('metadata.component: version', '1.0.0', str_val)

        call jf%destroy()
        deallocate(graph%nodes)
    end subroutine

    ! ---- components array ----

    subroutine test_sbom_components()
        type(dep_graph_t) :: graph
        character(len=:), allocatable :: json_output
        type(json_file) :: jf
        character(len=:), allocatable :: str_val
        logical :: found
        integer :: ierr, count

        call build_mock_graph(graph)
        call generate_sbom(graph, json_output, ierr)

        call jf%initialize()
        call jf%deserialize(json_output)

        call jf%info('components', found, var_type=count)
        call assert_true('components: array found', found)

        ! Components should have 2 entries (nodes 2 and 3, not the root)
        call jf%get('components(1).name', str_val, found)
        call assert_true('components(1): name found', found)
        call assert_equals_str('components(1): name', 'dep-github', str_val)

        call jf%get('components(1).type', str_val, found)
        call assert_equals_str('components(1): type', 'library', str_val)

        call jf%get('components(2).name', str_val, found)
        call assert_true('components(2): name found', found)
        call assert_equals_str('components(2): name', 'dep-generic', str_val)

        call jf%destroy()
        deallocate(graph%nodes)
    end subroutine

    ! ---- dependencies array ----

    subroutine test_sbom_dependencies()
        type(dep_graph_t) :: graph
        character(len=:), allocatable :: json_output
        type(json_file) :: jf
        character(len=:), allocatable :: str_val
        logical :: found
        integer :: ierr

        call build_mock_graph(graph)
        call generate_sbom(graph, json_output, ierr)

        call jf%initialize()
        call jf%deserialize(json_output)

        ! 3 nodes => 3 dependency entries
        call jf%get('dependencies(1).ref', str_val, found)
        call assert_true('dependencies(1): ref found', found)

        call jf%get('dependencies(2).ref', str_val, found)
        call assert_true('dependencies(2): ref found', found)

        call jf%get('dependencies(3).ref', str_val, found)
        call assert_true('dependencies(3): ref found', found)

        call jf%destroy()
        deallocate(graph%nodes)
    end subroutine

    ! ---- PURL values ----

    subroutine test_sbom_purls()
        type(dep_graph_t) :: graph
        character(len=:), allocatable :: json_output
        type(json_file) :: jf
        character(len=:), allocatable :: str_val
        logical :: found
        integer :: ierr

        call build_mock_graph(graph)
        call generate_sbom(graph, json_output, ierr)

        call jf%initialize()
        call jf%deserialize(json_output)

        ! Component 1 (dep-github) should have pkg:github/ purl
        call jf%get('components(1).purl', str_val, found)
        call assert_true('purl: github dep found', found)
        call assert_true('purl: github dep contains pkg:github/', &
            index(str_val, 'pkg:github/') > 0)

        ! Component 2 (dep-generic) should have pkg:generic/ purl
        call jf%get('components(2).purl', str_val, found)
        call assert_true('purl: generic dep found', found)
        call assert_true('purl: generic dep contains pkg:generic/', &
            index(str_val, 'pkg:generic/') > 0)

        call jf%destroy()
        deallocate(graph%nodes)
    end subroutine

end program test_generator
