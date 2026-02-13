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

module command_module
    use string_utils, only: starts_with
    implicit none
    private
    public :: run_command, git_clone, make_temp_dir, remove_dir, read_file_contents

contains

    !> Execute a shell command and capture its output
    subroutine run_command(cmd, output, exit_code)
        character(len=*), intent(in) :: cmd
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=256) :: tmp_file
        integer :: unit_num, io_stat
        character(len=4096) :: line
        character(len=:), allocatable :: accumulated

        ! Create a temp file for output capture
        tmp_file = '/tmp/cdx_fpm_cmd_output.tmp'

        call execute_command_line(cmd // ' > ' // trim(tmp_file) // ' 2>&1', &
                                  exitstat=exit_code)

        ! Read the output file
        accumulated = ''
        open(newunit=unit_num, file=trim(tmp_file), status='old', iostat=io_stat)
        if (io_stat == 0) then
            do
                read(unit_num, '(A)', iostat=io_stat) line
                if (io_stat /= 0) exit
                if (len_trim(accumulated) > 0) then
                    accumulated = accumulated // new_line('a') // trim(line)
                else
                    accumulated = trim(line)
                end if
            end do
            close(unit_num)
        end if

        output = accumulated

        ! Clean up temp file
        call execute_command_line('rm -f ' // trim(tmp_file), exitstat=io_stat)
    end subroutine run_command

    !> Clone a git repository with depth 1
    subroutine git_clone(url, dest_dir, tag, branch, rev, ierr)
        character(len=*), intent(in) :: url, dest_dir
        character(len=*), intent(in), optional :: tag, branch, rev
        integer, intent(out) :: ierr

        character(len=:), allocatable :: cmd, output
        character(len=:), allocatable :: ref_arg

        ref_arg = ''

        if (present(tag)) then
            if (len_trim(tag) > 0) then
                ref_arg = ' --branch ' // trim(tag)
            end if
        end if

        if (len_trim(ref_arg) == 0 .and. present(branch)) then
            if (len_trim(branch) > 0) then
                ref_arg = ' --branch ' // trim(branch)
            end if
        end if

        if (present(rev) .and. len_trim(ref_arg) == 0) then
            ! For specific revisions, clone then checkout
            cmd = 'git clone --quiet ' // trim(url) // ' ' // trim(dest_dir)
            call run_command(cmd, output, ierr)
            if (ierr /= 0) return

            cmd = 'cd ' // trim(dest_dir) // ' && git checkout --quiet ' // trim(rev)
            call run_command(cmd, output, ierr)
            return
        end if

        cmd = 'git clone --depth 1 --quiet' // ref_arg // ' ' // trim(url) // ' ' // trim(dest_dir)
        call run_command(cmd, output, ierr)
    end subroutine git_clone

    !> Create a temporary directory and return its path
    subroutine make_temp_dir(prefix, dir_path, ierr)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable, intent(out) :: dir_path
        integer, intent(out) :: ierr

        character(len=:), allocatable :: output

        call run_command('mktemp -d /tmp/' // trim(prefix) // '_XXXXXX', output, ierr)
        if (ierr == 0) then
            dir_path = trim(output)
        end if
    end subroutine make_temp_dir

    !> Remove a directory (with safety check for /tmp/ prefix)
    subroutine remove_dir(dir_path, ierr)
        character(len=*), intent(in) :: dir_path
        integer, intent(out) :: ierr

        character(len=:), allocatable :: output

        ! Safety: only remove directories under /tmp/
        if (.not. starts_with(trim(dir_path), '/tmp/')) then
            ierr = 1
            return
        end if

        call run_command('rm -rf ' // trim(dir_path), output, ierr)
    end subroutine remove_dir

    !> Read entire file contents into a string
    subroutine read_file_contents(filepath, contents, ierr)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: contents
        integer, intent(out) :: ierr

        integer :: unit_num, io_stat, file_size
        character(len=4096) :: line
        logical :: exists

        inquire(file=filepath, exist=exists)
        if (.not. exists) then
            ierr = 1
            contents = ''
            return
        end if

        contents = ''
        open(newunit=unit_num, file=filepath, status='old', iostat=io_stat)
        if (io_stat /= 0) then
            ierr = io_stat
            return
        end if

        do
            read(unit_num, '(A)', iostat=io_stat) line
            if (io_stat /= 0) exit
            if (len_trim(contents) > 0) then
                contents = contents // new_line('a') // trim(line)
            else
                contents = trim(line)
            end if
        end do

        close(unit_num)
        ierr = 0
    end subroutine read_file_contents

end module command_module
