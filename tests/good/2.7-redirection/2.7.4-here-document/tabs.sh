# 2.7.4: If the redirection operator is "<<-", all leading <tab>
# characters shall be stripped from input lines and the line containing
# the trailing delimiter.

cat <<EOF
a
	a
		a
EOF

cat <<-EOF
a
	a
		a
			EOF
