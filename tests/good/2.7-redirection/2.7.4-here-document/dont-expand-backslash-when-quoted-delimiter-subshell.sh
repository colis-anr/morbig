# from the SLE 2017 review:
# The delimiting word is quoted, so that the here-document lines
# should not be expanded (§2.7.4).
# This must also work in a subshell.

x=$(cat <<'EOT'
abc ` def
ghi \
jkl
EOT
)
