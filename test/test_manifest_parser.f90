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

program test_manifest_parser
    use manifest_parser
    implicit none

    integer :: failures
    character(len=256) :: tmp_dir, tmp_file
    integer :: u, ios, tlen

    failures = 0

    ! Get temp directory (cross-platform)
    call get_environment_variable('TMPDIR', tmp_dir, tlen, ios)
    if (ios /= 0 .or. tlen == 0) then
        call get_environment_variable('TEMP', tmp_dir, tlen, ios)
    end if
    if (ios /= 0 .or. tlen == 0) then
        call get_environment_variable('TMP', tmp_dir, tlen, ios)
    end if
    if (ios /= 0 .or. tlen == 0) then
        tmp_dir = '/tmp'
        tlen = 4
    end if

    ! Build temp file path with correct separator
    tmp_file = trim(tmp_dir) // '/test_cdx_fpm_manifest.toml'
    open(newunit=u, file=trim(tmp_file), status='replace', iostat=ios)
    if (ios /= 0) then
        write(0, '(a)') 'ERROR: Could not create temp fixture file'
        error stop 1
    end if
    write(u, '(a)') 'name = "my-test-project"'
    write(u, '(a)') 'version = "1.2.3"'
    write(u, '(a)') 'license = "MIT"'
    write(u, '(a)') 'author = "Test Author"'
    write(u, '(a)') 'description = "A test project"'
    write(u, '(a)') ''
    write(u, '(a)') '[dependencies]'
    write(u, '(a)') '[dependencies.dep-git1]'
    write(u, '(a)') 'git = "https://github.com/owner1/repo1"'
    write(u, '(a)') '[dependencies.dep-git2]'
    write(u, '(a)') 'git = "https://github.com/owner2/repo2"'
    write(u, '(a)') 'tag = "v2.0"'
    write(u, '(a)') '[dependencies.dep-path1]'
    write(u, '(a)') 'path = "../local-dep"'
    write(u, '(a)') ''
    write(u, '(a)') '[dev-dependencies]'
    write(u, '(a)') '[dev-dependencies.test-dep]'
    write(u, '(a)') 'git = "https://github.com/tester/testlib"'
    close(u)

    ! Run tests
    call test_parse_scalars()
    call test_parse_dependencies()
    call test_parse_dev_dependencies()

    ! Clean up
    open(newunit=u, file=trim(tmp_file), status='old', iostat=ios)
    if (ios == 0) close(u, status='delete')

    if (failures > 0) then
        write(0, '(a,i0,a)') 'FAILED: ', failures, ' test(s) failed'
        error stop 1
    else
        write(*, '(a)') 'All manifest_parser tests passed.'
    end if

contains

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

    ! ---- Scalar fields ----

    subroutine test_parse_scalars()
        type(package_t) :: pkg
        integer :: ierr

        call parse_manifest_file(trim(tmp_file), pkg, ierr)
        call assert_equals_int('parse_scalars: ierr == 0', 0, ierr)
        call assert_equals_str('parse_scalars: name', 'my-test-project', pkg%name)
        call assert_equals_str('parse_scalars: version', '1.2.3', pkg%version)
        call assert_equals_str('parse_scalars: license', 'MIT', pkg%license)
        call assert_equals_str('parse_scalars: author', 'Test Author', pkg%author)
        call assert_equals_str('parse_scalars: description', 'A test project', pkg%description)
    end subroutine

    ! ---- Dependencies ----

    subroutine test_parse_dependencies()
        type(package_t) :: pkg
        integer :: ierr, i
        logical :: found_git1, found_git2, found_path1

        call parse_manifest_file(trim(tmp_file), pkg, ierr)
        call assert_equals_int('parse_deps: ierr == 0', 0, ierr)
        call assert_equals_int('parse_deps: dep_count', 3, pkg%dep_count)

        found_git1 = .false.
        found_git2 = .false.
        found_path1 = .false.

        do i = 1, pkg%dep_count
            select case (trim(pkg%dependencies(i)%name))
            case ('dep-git1')
                found_git1 = .true.
                call assert_equals_int('dep-git1: source_kind', SOURCE_GIT, &
                    pkg%dependencies(i)%source_kind)
                call assert_equals_str('dep-git1: git_url', &
                    'https://github.com/owner1/repo1', pkg%dependencies(i)%git_url)
            case ('dep-git2')
                found_git2 = .true.
                call assert_equals_int('dep-git2: source_kind', SOURCE_GIT, &
                    pkg%dependencies(i)%source_kind)
                call assert_equals_str('dep-git2: git_url', &
                    'https://github.com/owner2/repo2', pkg%dependencies(i)%git_url)
                call assert_equals_str('dep-git2: git_tag', 'v2.0', &
                    pkg%dependencies(i)%git_tag)
            case ('dep-path1')
                found_path1 = .true.
                call assert_equals_int('dep-path1: source_kind', SOURCE_PATH, &
                    pkg%dependencies(i)%source_kind)
                call assert_equals_str('dep-path1: path', '../local-dep', &
                    pkg%dependencies(i)%path)
            end select
        end do

        call assert_true('parse_deps: found dep-git1', found_git1)
        call assert_true('parse_deps: found dep-git2', found_git2)
        call assert_true('parse_deps: found dep-path1', found_path1)
    end subroutine

    ! ---- Dev dependencies ----

    subroutine test_parse_dev_dependencies()
        type(package_t) :: pkg
        integer :: ierr

        call parse_manifest_file(trim(tmp_file), pkg, ierr)
        call assert_equals_int('parse_dev_deps: ierr == 0', 0, ierr)
        call assert_equals_int('parse_dev_deps: dev_dep_count', 1, pkg%dev_dep_count)
        call assert_equals_str('parse_dev_deps: name', 'test-dep', &
            pkg%dev_dependencies(1)%name)
        call assert_equals_int('parse_dev_deps: source_kind', SOURCE_GIT, &
            pkg%dev_dependencies(1)%source_kind)
        call assert_equals_str('parse_dev_deps: git_url', &
            'https://github.com/tester/testlib', pkg%dev_dependencies(1)%git_url)
    end subroutine

end program test_manifest_parser
