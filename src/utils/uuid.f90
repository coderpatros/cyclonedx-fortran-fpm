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

module uuid_module
    implicit none
    private
    public :: generate_uuid4

contains

    !> Generate a UUID v4 string (random-based)
    !> Format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
    !> where y is one of 8, 9, a, b
    subroutine generate_uuid4(uuid_str)
        character(len=36), intent(out) :: uuid_str
        integer :: values(8), i
        real :: r
        integer :: bytes(16)
        character(len=16), parameter :: hex = '0123456789abcdef'

        ! Seed random number generator from clock
        call date_and_time(values=values)
        call random_seed()

        ! Generate 16 random bytes
        do i = 1, 16
            call random_number(r)
            bytes(i) = int(r * 256.0)
            if (bytes(i) > 255) bytes(i) = 255
        end do

        ! Set version to 4 (byte 7, high nibble)
        bytes(7) = ior(iand(bytes(7), 15), 64)  ! 0100xxxx

        ! Set variant to RFC 4122 (byte 9, high bits = 10)
        bytes(9) = ior(iand(bytes(9), 63), 128) ! 10xxxxxx

        ! Format as UUID string
        write(uuid_str, '(a2,a2,a2,a2,a1,a2,a2,a1,a2,a2,a1,a2,a2,a1,a2,a2,a2,a2,a2,a2)') &
            hex_byte(bytes(1)), hex_byte(bytes(2)), hex_byte(bytes(3)), hex_byte(bytes(4)), &
            '-', &
            hex_byte(bytes(5)), hex_byte(bytes(6)), &
            '-', &
            hex_byte(bytes(7)), hex_byte(bytes(8)), &
            '-', &
            hex_byte(bytes(9)), hex_byte(bytes(10)), &
            '-', &
            hex_byte(bytes(11)), hex_byte(bytes(12)), hex_byte(bytes(13)), &
            hex_byte(bytes(14)), hex_byte(bytes(15)), hex_byte(bytes(16))
    end subroutine generate_uuid4

    !> Convert a byte (0-255) to a 2-character hex string
    function hex_byte(b) result(hx)
        integer, intent(in) :: b
        character(len=2) :: hx
        character(len=16), parameter :: hex = '0123456789abcdef'
        integer :: hi, lo

        hi = ishft(iand(b, 240), -4) + 1
        lo = iand(b, 15) + 1
        hx(1:1) = hex(hi:hi)
        hx(2:2) = hex(lo:lo)
    end function hex_byte

end module uuid_module
