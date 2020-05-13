# Run with heredoc as an argument
#
# Example:
#
#   rails_run <<END
#     # This prints Hello world
#     puts 'Hello, world!'
#   END
#
#   # Or
#
#   rails_run puts \'Hello world\'
#
function rails_run {
  local code

  # Read Heredoc
  IFS='' read -r -t 0 -d '' code || code="$*"

  echo 'Executing: '
  echo "$code"

  # Execute in rails console. Skip empty lines and comments
  echo "$code" | awk 'NF' | grep -v '^#' | bundle exec rails console
}
