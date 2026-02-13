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

module manifest_parser
    use tomlf, only: toml_table, toml_parse, toml_array, get_value, toml_key, &
                     toml_error
    use string_utils, only: join_path
    implicit none
    private

    public :: dependency_t, package_t, parse_manifest, parse_manifest_file

    integer, parameter, public :: SOURCE_GIT = 1
    integer, parameter, public :: SOURCE_PATH = 2
    integer, parameter, public :: SOURCE_REGISTRY = 3

    type :: dependency_t
        character(len=256) :: name = ''
        integer :: source_kind = 0
        character(len=512) :: git_url = ''
        character(len=128) :: git_tag = ''
        character(len=128) :: git_branch = ''
        character(len=128) :: git_rev = ''
        character(len=512) :: path = ''
        character(len=128) :: version_spec = ''
    end type dependency_t

    type :: package_t
        character(len=256) :: name = ''
        character(len=64)  :: version = ''
        character(len=128) :: license = ''
        character(len=256) :: author = ''
        character(len=512) :: description = ''
        character(len=512) :: homepage = ''
        type(dependency_t), allocatable :: dependencies(:)
        integer :: dep_count = 0
        type(dependency_t), allocatable :: dev_dependencies(:)
        integer :: dev_dep_count = 0
    end type package_t

contains

    !> Parse an fpm.toml file at the given path
    subroutine parse_manifest_file(filepath, pkg, ierr)
        character(len=*), intent(in) :: filepath
        type(package_t), intent(out) :: pkg
        integer, intent(out) :: ierr

        type(toml_table), allocatable :: table
        type(toml_error), allocatable :: error
        integer :: unit_num, io_stat

        open(newunit=unit_num, file=filepath, status='old', iostat=io_stat)
        if (io_stat /= 0) then
            ierr = 1
            return
        end if

        call toml_parse(table, unit_num, error)
        close(unit_num)
        if (allocated(error)) then
            ierr = 1
            return
        end if

        call parse_manifest(table, pkg, ierr)
    end subroutine parse_manifest_file

    !> Parse a TOML table into a package_t
    subroutine parse_manifest(table, pkg, ierr)
        type(toml_table), intent(inout) :: table
        type(package_t), intent(out) :: pkg
        integer, intent(out) :: ierr

        type(toml_table), pointer :: dep_table, dev_dep_table

        ierr = 0

        ! Read top-level fields
        call get_string(table, 'name', pkg%name)
        call get_string(table, 'version', pkg%version)
        call get_string(table, 'license', pkg%license)
        call get_string(table, 'author', pkg%author)
        call get_string(table, 'description', pkg%description)
        call get_string(table, 'homepage', pkg%homepage)

        ! Parse [dependencies]
        call get_value(table, 'dependencies', dep_table)
        if (associated(dep_table)) then
            call parse_dep_table(dep_table, pkg%dependencies, pkg%dep_count, ierr)
            if (ierr /= 0) return
        else
            allocate(pkg%dependencies(0))
        end if

        ! Parse [dev-dependencies]
        call get_value(table, 'dev-dependencies', dev_dep_table)
        if (associated(dev_dep_table)) then
            call parse_dep_table(dev_dep_table, pkg%dev_dependencies, pkg%dev_dep_count, ierr)
            if (ierr /= 0) return
        else
            allocate(pkg%dev_dependencies(0))
        end if
    end subroutine parse_manifest

    !> Parse a dependency table section
    subroutine parse_dep_table(table, deps, count, ierr)
        type(toml_table), intent(inout) :: table
        type(dependency_t), allocatable, intent(out) :: deps(:)
        integer, intent(out) :: count, ierr

        type(toml_table), pointer :: dep_entry
        type(toml_key), allocatable :: keys(:)
        integer :: i, n

        ierr = 0
        count = 0

        call table%get_keys(keys)
        n = size(keys)

        allocate(deps(n))

        do i = 1, n
            count = count + 1
            deps(count)%name = keys(i)%key

            call get_value(table, keys(i)%key, dep_entry)
            if (.not. associated(dep_entry)) cycle

            ! Check for git dependency
            call get_string(dep_entry, 'git', deps(count)%git_url)
            if (len_trim(deps(count)%git_url) > 0) then
                deps(count)%source_kind = SOURCE_GIT
                call get_string(dep_entry, 'tag', deps(count)%git_tag)
                call get_string(dep_entry, 'branch', deps(count)%git_branch)
                call get_string(dep_entry, 'rev', deps(count)%git_rev)
                cycle
            end if

            ! Check for path dependency
            call get_string(dep_entry, 'path', deps(count)%path)
            if (len_trim(deps(count)%path) > 0) then
                deps(count)%source_kind = SOURCE_PATH
                cycle
            end if

            ! Otherwise treat as registry dependency
            deps(count)%source_kind = SOURCE_REGISTRY
            call get_string(dep_entry, 'version', deps(count)%version_spec)
        end do
    end subroutine parse_dep_table

    !> Helper to safely get a string value from a TOML table
    subroutine get_string(table, key, val)
        type(toml_table), intent(inout) :: table
        character(len=*), intent(in) :: key
        character(len=*), intent(out) :: val
        character(len=:), allocatable :: tmp

        val = ''
        call get_value(table, key, tmp)
        if (allocated(tmp)) then
            val = tmp
        end if
    end subroutine get_string

end module manifest_parser
