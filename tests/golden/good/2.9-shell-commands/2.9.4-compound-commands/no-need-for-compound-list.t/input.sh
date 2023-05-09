# Quoting Jilles Tjoelker from his message <20170612220234.GA26233@stack.nl>
# sent to austin-group-l@opengroup.org on  Mon, 12 Jun 2017.

until { false; } do echo hi; break; done

until(false)do echo hi; break; done

if :; then if :; then echo hi; fi fi

{ { echo hi; } }
