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

program cyclonedx_fpm_cli
    use dependency_resolver, only: dep_graph_t, resolve_dependencies
    use cyclonedx_generator, only: generate_sbom
    implicit none

    character(len=512) :: project_path
    character(len=512) :: output_file
    logical :: include_dev
    logical :: show_help
    logical :: show_version
    type(dep_graph_t) :: graph
    character(len=:), allocatable :: json_output
    integer :: ierr, out_unit, io_stat

    ! Parse CLI arguments
    call parse_args(project_path, output_file, include_dev, show_help, show_version)

    if (show_help) then
        call print_help()
        stop
    end if

    if (show_version) then
        write(*, '(A)') 'cyclonedx-fpm 0.1.0'
        stop
    end if

    ! Resolve dependencies
    write(0, '(A)') 'Resolving dependencies...'
    call resolve_dependencies(trim(project_path), include_dev, graph, ierr)
    if (ierr /= 0) then
        write(0, '(A,I0)') 'Error resolving dependencies: ', ierr
        error stop 1
    end if

    write(0, '(A,I0,A)') 'Resolved ', graph%node_count - 1, ' dependencies.'

    ! Generate SBOM
    call generate_sbom(graph, json_output, ierr)
    if (ierr /= 0) then
        write(0, '(A,I0)') 'Error generating SBOM: ', ierr
        error stop 1
    end if

    ! Output
    if (len_trim(output_file) > 0) then
        open(newunit=out_unit, file=trim(output_file), status='replace', &
             action='write', iostat=io_stat)
        if (io_stat /= 0) then
            write(0, '(A,A)') 'Error opening output file: ', trim(output_file)
            error stop 1
        end if
        write(out_unit, '(A)') json_output
        close(out_unit)
        write(0, '(A,A)') 'SBOM written to ', trim(output_file)
    else
        write(*, '(A)') json_output
    end if

contains

    subroutine parse_args(project_path, output_file, include_dev, show_help, show_version)
        character(len=*), intent(out) :: project_path, output_file
        logical, intent(out) :: include_dev, show_help, show_version

        integer :: i, nargs
        character(len=512) :: arg
        logical :: next_is_output

        project_path = '.'
        output_file = ''
        include_dev = .false.
        show_help = .false.
        show_version = .false.
        next_is_output = .false.

        nargs = command_argument_count()

        do i = 1, nargs
            call get_command_argument(i, arg)

            if (next_is_output) then
                output_file = arg
                next_is_output = .false.
                cycle
            end if

            select case (trim(arg))
            case ('-h', '--help')
                show_help = .true.
                return
            case ('--version')
                show_version = .true.
                return
            case ('-o', '--output')
                next_is_output = .true.
            case ('--dev')
                include_dev = .true.
            case default
                ! Positional argument: project path
                if (arg(1:1) /= '-') then
                    project_path = arg
                else
                    write(0, '(A,A)') 'Unknown option: ', trim(arg)
                    show_help = .true.
                    return
                end if
            end select
        end do
    end subroutine parse_args

    subroutine print_help()
        write(*, '(A)') 'cyclonedx-fpm - CycloneDX SBOM generator for Fortran fpm projects'
        write(*, '(A)') ''
        write(*, '(A)') 'USAGE:'
        write(*, '(A)') '  cyclonedx-fpm [OPTIONS] [PROJECT_PATH]'
        write(*, '(A)') ''
        write(*, '(A)') 'ARGUMENTS:'
        write(*, '(A)') '  PROJECT_PATH  Path to fpm project (default: current directory)'
        write(*, '(A)') ''
        write(*, '(A)') 'OPTIONS:'
        write(*, '(A)') '  -o, --output FILE   Write SBOM to FILE (default: stdout)'
        write(*, '(A)') '  --dev               Include dev-dependencies'
        write(*, '(A)') '  -h, --help          Print help'
        write(*, '(A)') '  --version           Print version'
    end subroutine print_help

end program cyclonedx_fpm_cli
