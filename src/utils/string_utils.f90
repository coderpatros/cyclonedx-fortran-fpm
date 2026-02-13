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

module string_utils
    implicit none
    private
    public :: join_path, extract_github_owner_repo, to_lower, starts_with, &
              strip_trailing_slash, string_replace

contains

    !> Join two path segments with a separator
    function join_path(base, segment) result(path)
        character(len=*), intent(in) :: base, segment
        character(len=:), allocatable :: path
        character(len=1) :: sep

        sep = '/'
        if (len_trim(base) == 0) then
            path = trim(segment)
        else if (len_trim(segment) == 0) then
            path = trim(base)
        else if (base(len_trim(base):len_trim(base)) == sep) then
            path = trim(base) // trim(segment)
        else
            path = trim(base) // sep // trim(segment)
        end if
    end function join_path

    !> Extract owner/repo from a GitHub URL
    !> e.g. "https://github.com/owner/repo" -> owner="owner", repo="repo"
    subroutine extract_github_owner_repo(url, owner, repo, is_github)
        character(len=*), intent(in) :: url
        character(len=:), allocatable, intent(out) :: owner, repo
        logical, intent(out) :: is_github

        character(len=:), allocatable :: work
        integer :: idx, slash_pos

        is_github = .false.
        owner = ''
        repo = ''

        ! Check if it's a GitHub URL
        if (index(url, 'github.com') == 0) return

        is_github = .true.

        ! Find the part after github.com/
        idx = index(url, 'github.com/') + len('github.com/')
        if (idx > len_trim(url)) return

        work = trim(url(idx:))

        ! Strip trailing .git
        if (len_trim(work) > 4) then
            if (work(len_trim(work)-3:len_trim(work)) == '.git') then
                work = work(1:len_trim(work)-4)
            end if
        end if

        ! Strip trailing slash
        work = strip_trailing_slash(work)

        ! Split on /
        slash_pos = index(work, '/')
        if (slash_pos == 0) return

        owner = work(1:slash_pos-1)
        repo = work(slash_pos+1:)

        ! Strip any further slashes from repo
        slash_pos = index(repo, '/')
        if (slash_pos > 0) then
            repo = repo(1:slash_pos-1)
        end if
    end subroutine extract_github_owner_repo

    !> Convert string to lowercase
    function to_lower(str) result(lower)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower
        integer :: i, ic

        allocate(character(len=len(str)) :: lower)
        do i = 1, len(str)
            ic = iachar(str(i:i))
            if (ic >= iachar('A') .and. ic <= iachar('Z')) then
                lower(i:i) = achar(ic + 32)
            else
                lower(i:i) = str(i:i)
            end if
        end do
    end function to_lower

    !> Check if string starts with prefix
    pure logical function starts_with(str, prefix)
        character(len=*), intent(in) :: str, prefix

        if (len(prefix) > len(str)) then
            starts_with = .false.
        else
            starts_with = str(1:len(prefix)) == prefix
        end if
    end function starts_with

    !> Strip trailing slash from a string
    function strip_trailing_slash(str) result(res)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: res

        if (len_trim(str) > 0 .and. str(len_trim(str):len_trim(str)) == '/') then
            res = str(1:len_trim(str)-1)
        else
            res = trim(str)
        end if
    end function strip_trailing_slash

    !> Replace all occurrences of 'old' with 'new_str' in 'str'
    function string_replace(str, old, new_str) result(res)
        character(len=*), intent(in) :: str, old, new_str
        character(len=:), allocatable :: res
        integer :: idx, pos

        res = str
        pos = 1
        do
            idx = index(res(pos:), old)
            if (idx == 0) exit
            idx = idx + pos - 1
            res = res(1:idx-1) // new_str // res(idx+len(old):)
            pos = idx + len(new_str)
        end do
    end function string_replace

end module string_utils
