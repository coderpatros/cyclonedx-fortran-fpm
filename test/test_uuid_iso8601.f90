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

program test_uuid_iso8601
    use uuid_module
    use iso8601_module
    implicit none

    integer :: failures

    failures = 0

    ! UUID tests
    call test_uuid_length()
    call test_uuid_hyphens()
    call test_uuid_version()
    call test_uuid_variant()
    call test_uuid_uniqueness()

    ! ISO 8601 tests
    call test_timestamp_length()
    call test_timestamp_format()

    if (failures > 0) then
        write(0, '(a,i0,a)') 'FAILED: ', failures, ' test(s) failed'
        error stop 1
    else
        write(*, '(a)') 'All uuid_iso8601 tests passed.'
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

    ! ---- UUID tests ----

    subroutine test_uuid_length()
        character(len=36) :: uuid
        call generate_uuid4(uuid)
        call assert_true('uuid: length is 36', len_trim(uuid) == 36)
    end subroutine

    subroutine test_uuid_hyphens()
        character(len=36) :: uuid
        call generate_uuid4(uuid)
        call assert_true('uuid: hyphen at pos 9', uuid(9:9) == '-')
        call assert_true('uuid: hyphen at pos 14', uuid(14:14) == '-')
        call assert_true('uuid: hyphen at pos 19', uuid(19:19) == '-')
        call assert_true('uuid: hyphen at pos 24', uuid(24:24) == '-')
    end subroutine

    subroutine test_uuid_version()
        character(len=36) :: uuid
        call generate_uuid4(uuid)
        call assert_true('uuid: version char is 4', uuid(15:15) == '4')
    end subroutine

    subroutine test_uuid_variant()
        character(len=36) :: uuid
        character(len=1) :: c
        call generate_uuid4(uuid)
        c = uuid(20:20)
        call assert_true('uuid: variant char in {8,9,a,b}', &
            c == '8' .or. c == '9' .or. c == 'a' .or. c == 'b')
    end subroutine

    subroutine test_uuid_uniqueness()
        character(len=36) :: uuid1, uuid2
        call generate_uuid4(uuid1)
        call generate_uuid4(uuid2)
        call assert_true('uuid: two calls produce different UUIDs', uuid1 /= uuid2)
    end subroutine

    ! ---- ISO 8601 timestamp tests ----

    subroutine test_timestamp_length()
        character(len=20) :: ts
        call get_iso8601_timestamp(ts)
        call assert_true('timestamp: length is 20', len_trim(ts) == 20)
    end subroutine

    subroutine test_timestamp_format()
        character(len=20) :: ts
        call get_iso8601_timestamp(ts)
        call assert_true('timestamp: T at pos 11', ts(11:11) == 'T')
        call assert_true('timestamp: Z at pos 20', ts(20:20) == 'Z')
        call assert_true('timestamp: dash at pos 5', ts(5:5) == '-')
        call assert_true('timestamp: dash at pos 8', ts(8:8) == '-')
        call assert_true('timestamp: colon at pos 14', ts(14:14) == ':')
        call assert_true('timestamp: colon at pos 17', ts(17:17) == ':')
    end subroutine

end program test_uuid_iso8601
