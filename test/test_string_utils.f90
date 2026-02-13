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

program test_string_utils
    use string_utils
    implicit none

    integer :: failures

    failures = 0

    ! join_path tests
    call test_join_path_normal()
    call test_join_path_trailing_slash()
    call test_join_path_empty_segments()

    ! extract_github_owner_repo tests
    call test_extract_github_standard()
    call test_extract_github_git_suffix()
    call test_extract_non_github()

    ! to_lower tests
    call test_to_lower_mixed()
    call test_to_lower_already()

    ! starts_with tests
    call test_starts_with_match()
    call test_starts_with_no_match()
    call test_starts_with_prefix_longer()

    ! strip_trailing_slash tests
    call test_strip_trailing_slash_with()
    call test_strip_trailing_slash_without()

    ! string_replace tests
    call test_string_replace_single()
    call test_string_replace_multiple()
    call test_string_replace_no_match()

    if (failures > 0) then
        write(0, '(a,i0,a)') 'FAILED: ', failures, ' test(s) failed'
        error stop 1
    else
        write(*, '(a)') 'All string_utils tests passed.'
    end if

contains

    subroutine assert_equals_str(test_name, expected, actual)
        character(len=*), intent(in) :: test_name, expected, actual
        if (trim(expected) == trim(actual)) then
            write(*, '(a,a,a)') 'PASS: ', test_name, ''
        else
            write(*, '(a,a)') 'FAIL: ', test_name
            write(*, '(a,a,a)') '  expected: "', trim(expected), '"'
            write(*, '(a,a,a)') '  actual:   "', trim(actual), '"'
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

    ! ---- join_path tests ----

    subroutine test_join_path_normal()
        character(len=:), allocatable :: res
        res = join_path('/home/user', 'project')
        call assert_equals_str('join_path: normal', '/home/user/project', res)
    end subroutine

    subroutine test_join_path_trailing_slash()
        character(len=:), allocatable :: res
        res = join_path('/home/user/', 'project')
        call assert_equals_str('join_path: trailing slash base', '/home/user/project', res)
    end subroutine

    subroutine test_join_path_empty_segments()
        character(len=:), allocatable :: res
        res = join_path('', 'project')
        call assert_equals_str('join_path: empty base', 'project', res)
        res = join_path('/home', '')
        call assert_equals_str('join_path: empty segment', '/home', res)
    end subroutine

    ! ---- extract_github_owner_repo tests ----

    subroutine test_extract_github_standard()
        character(len=:), allocatable :: owner, repo
        logical :: is_github
        call extract_github_owner_repo('https://github.com/owner/repo', owner, repo, is_github)
        call assert_true('extract_github: is_github', is_github)
        call assert_equals_str('extract_github: owner', 'owner', owner)
        call assert_equals_str('extract_github: repo', 'repo', repo)
    end subroutine

    subroutine test_extract_github_git_suffix()
        character(len=:), allocatable :: owner, repo
        logical :: is_github
        call extract_github_owner_repo('https://github.com/foo/bar.git', owner, repo, is_github)
        call assert_true('extract_github .git: is_github', is_github)
        call assert_equals_str('extract_github .git: owner', 'foo', owner)
        call assert_equals_str('extract_github .git: repo', 'bar', repo)
    end subroutine

    subroutine test_extract_non_github()
        character(len=:), allocatable :: owner, repo
        logical :: is_github
        call extract_github_owner_repo('https://gitlab.com/foo/bar', owner, repo, is_github)
        call assert_true('extract_github: non-github', .not. is_github)
    end subroutine

    ! ---- to_lower tests ----

    subroutine test_to_lower_mixed()
        character(len=:), allocatable :: res
        res = to_lower('Hello World')
        call assert_equals_str('to_lower: mixed case', 'hello world', res)
    end subroutine

    subroutine test_to_lower_already()
        character(len=:), allocatable :: res
        res = to_lower('already')
        call assert_equals_str('to_lower: already lowercase', 'already', res)
    end subroutine

    ! ---- starts_with tests ----

    subroutine test_starts_with_match()
        call assert_true('starts_with: match', starts_with('hello world', 'hello'))
    end subroutine

    subroutine test_starts_with_no_match()
        call assert_true('starts_with: no match', .not. starts_with('hello world', 'world'))
    end subroutine

    subroutine test_starts_with_prefix_longer()
        call assert_true('starts_with: prefix longer', .not. starts_with('hi', 'hello'))
    end subroutine

    ! ---- strip_trailing_slash tests ----

    subroutine test_strip_trailing_slash_with()
        character(len=:), allocatable :: res
        res = strip_trailing_slash('/home/user/')
        call assert_equals_str('strip_trailing_slash: with slash', '/home/user', res)
    end subroutine

    subroutine test_strip_trailing_slash_without()
        character(len=:), allocatable :: res
        res = strip_trailing_slash('/home/user')
        call assert_equals_str('strip_trailing_slash: without slash', '/home/user', res)
    end subroutine

    ! ---- string_replace tests ----

    subroutine test_string_replace_single()
        character(len=:), allocatable :: res
        res = string_replace('hello world', 'world', 'fortran')
        call assert_equals_str('string_replace: single', 'hello fortran', res)
    end subroutine

    subroutine test_string_replace_multiple()
        character(len=:), allocatable :: res
        res = string_replace('aabaa', 'a', 'x')
        call assert_equals_str('string_replace: multiple', 'xxbxx', res)
    end subroutine

    subroutine test_string_replace_no_match()
        character(len=:), allocatable :: res
        res = string_replace('hello', 'xyz', 'abc')
        call assert_equals_str('string_replace: no match', 'hello', res)
    end subroutine

end program test_string_utils
