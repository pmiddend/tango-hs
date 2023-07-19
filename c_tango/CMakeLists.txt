cmake_minimum_required(VERSION 3.10)

project(ctango VERSION 1.0)

set(CMAKE_CXX_STANDARD 14)

# see https://stackoverflow.com/questions/29191855/what-is-the-proper-way-to-use-pkg-config-from-cmake
find_package(PkgConfig REQUIRED)
pkg_check_modules(TANGO
  REQUIRED
  tango
)

add_library(ctango SHARED
  src/c_tango_attribute.cpp
  src/c_tango_command.cpp
  src/c_tango_dbase.cpp
  src/c_tango_proxy.cpp
)
target_include_directories(ctango PUBLIC src ${TANGO_INCLUDE_DIRS})
target_link_libraries(ctango ${TANGO_LIBRARIES})
configure_file(ctango.pc.in ctango.pc @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/ctango.pc"
        DESTINATION lib/pkgconfig)


install(TARGETS ctango DESTINATION lib)
install(FILES "src/c_tango.h" DESTINATION include)
