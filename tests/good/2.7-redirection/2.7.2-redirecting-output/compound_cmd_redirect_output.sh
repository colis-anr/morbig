cat > /etc/ca-certificates.conf <<EOF
# This file lists certificates that you wish to use or to ignore to be
EOF
(echo $CERTS_ENABLED) >> /etc/ca-certificates.conf


