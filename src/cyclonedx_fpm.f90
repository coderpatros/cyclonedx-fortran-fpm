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

module cyclonedx_fpm
    !! Top-level re-export module for cyclonedx-fpm
    use manifest_parser, only: dependency_t, package_t, parse_manifest_file, &
                                SOURCE_GIT, SOURCE_PATH, SOURCE_REGISTRY
    use dependency_resolver, only: dep_node_t, dep_graph_t, resolve_dependencies
    use cyclonedx_generator, only: generate_sbom
    use string_utils, only: join_path, extract_github_owner_repo
    use uuid_module, only: generate_uuid4
    use iso8601_module, only: get_iso8601_timestamp
    use command_module, only: run_command, git_clone, make_temp_dir, remove_dir
    implicit none
end module cyclonedx_fpm
