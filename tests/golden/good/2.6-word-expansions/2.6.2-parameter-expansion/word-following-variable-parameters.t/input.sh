# Reported by @jwilk

echo ${1#;}

# Reported by @niols

echo ${#*}

echo ${#@}

echo ${#foo}

# Extra tests

echo ${1#

}

echo ${1#}

echo ${1#'}'}

echo ${1#"}"}

echo ${1#\}}

echo ${1#"foo}"}

echo ${1#for}

echo ${1#0>}
