let andpath="~/androidrc/frameworks/base/core/"
exec "set path+=".andpath."**"
exec "cscope add ".andpath."cscope.out ".andpath
exec "set tags+=".andpath."tags,".andpath."TAGS"

