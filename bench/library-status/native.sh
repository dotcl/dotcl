# Sourced by run-quickload.sh and run-tests.sh: name the native library or
# header whose absence stopped a system, so a measurement can record it
# without treating a missing package on the host as a dotcl failure.
#
#   native_missing LOG   print the name, or nothing when the log does not show
#                        a missing foreign library or header

native_missing() {
  log=$1

  # cffi-grovel compiles a generated C file; the compiler names the header.
  header=$(sed -n 's/.*fatal error: \([^:]*\): No such file or directory.*/\1/p' "$log" | head -1)
  if [ -n "$header" ]; then
    printf '%s' "$header"
    return 0
  fi

  # cffi:load-foreign-library-error lists the alternatives it tried.
  alternatives=$(sed -n 's/.*Unable to load any of the alternatives: *(\(.*\)).*/\1/p' "$log" | head -1)
  if [ -n "$alternatives" ]; then
    case "$alternatives" in
      *'(:FRAMEWORK '*)
        printf '%s' "$alternatives" \
          | sed -n 's/.*(:FRAMEWORK \([^)]*\)).*/\1/p' | head -1
        return 0 ;;
      *'(:DEFAULT '*)
        printf '%s' "$alternatives" \
          | sed -n 's/.*(:DEFAULT \([^)]*\)).*/\1/p' | head -1
        return 0 ;;
    esac
    set -- $alternatives
    printf '%s' "${1:-$alternatives}"
    return 0
  fi

  # A library named in the condition, rather than a list of alternatives.
  foreign=$(sed -n 's/.*Unable to load foreign library (\([^)]*\)).*/\1/p' "$log" | head -1)
  if [ -n "$foreign" ]; then
    printf '%s' "$foreign"
    return 0
  fi
  failed=$(sed -n 's/.*[Ff]ailed to load \(lib[A-Za-z0-9_.+-]*\).*/\1/p' "$log" | head -1)
  if [ -n "$failed" ]; then
    printf '%s' "${failed%[.,;:]}"
    return 0
  fi
  if grep -q 'SB-MPFR failed to load' "$log"; then
    printf 'libmpfr'
    return 0
  fi
  if grep -q 'CUDA SDK not found' "$log"; then
    printf 'libcuda'
    return 0
  fi

  # A file name next to "No such file or directory" is a missing header or
  # shared object even when no cffi condition wraps it.
  if grep -q 'No such file or directory' "$log"; then
    file=$(grep -aoE '[A-Za-z0-9_+.-]+\.(h|hpp|so|dylib|dll)' "$log" | head -1)
    if [ -n "$file" ]; then
      printf '%s' "$file"
      return 0
    fi
  fi

  # A failed cc run from cffi-grovel is a missing header or C library even
  # when the compiler output did not survive into the log.
  if grep -qE 'Subprocess.*with command .*cc ' "$log"; then
    printf 'C compiler'
    return 0
  fi
  if grep -q 'fatal error:' "$log"; then
    printf 'C compiler'
    return 0
  fi

  # The remaining cffi shapes name no library, only that loading one failed.
  if grep -qiE 'load-foreign-library|foreign library|could not load.*library' "$log"; then
    printf 'shared library'
    return 0
  fi
  if grep -qE 'Unable to load shared library|Error opening shared object|dlopen\(|Library not loaded|shared library not existing' "$log"; then
    printf 'shared library'
    return 0
  fi
}
