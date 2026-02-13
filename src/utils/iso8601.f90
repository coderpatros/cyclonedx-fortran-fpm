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

module iso8601_module
    implicit none
    private
    public :: get_iso8601_timestamp

contains

    !> Get current UTC timestamp in ISO 8601 format
    !> Format: YYYY-MM-DDThh:mm:ssZ
    subroutine get_iso8601_timestamp(timestamp)
        character(len=20), intent(out) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)

        write(timestamp, '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1)') &
            values(1), '-', values(2), '-', values(3), 'T', &
            values(5), ':', values(6), ':', values(7), 'Z'
    end subroutine get_iso8601_timestamp

end module iso8601_module
