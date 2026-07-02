ins <- ""

# print: "Who are you? Enter . to confirm."
ins <- paste0(ins, "87+.>104+.>111+.>32+.>97+.>114+.>101+.>32+.>121+.>111+.>117+.>63+.>10+.>69+.>110+.>116+.>101+.>114+.>32+.>34+.>46+.>34+.>32+.>116+.>111+.>32+.>99+.>111+.>110+.>102+.>105+.>114+.>109+.>46+.>")
# move ptr right to create an empty cell
ins <- paste0(ins, ">")
# save: "Hello, " but minus 46
ins <- paste0(ins, "26+>55+>62+>62+>65+>2->14-")
# repeat: read in char, minus 46 and move pointer right, until cell is zero (the "." char)
ins <- paste0(ins, "[>,46-]")
# move pointer left: to the empty cell created at step 2
ins <- paste0(ins, "<[<]")
# move pointer right: pointing at the "H" of "Hello, " (minused 46)
ins <- paste0(ins, ">")
# loop: increase cell by 46, print, then move pointer right, until cell is zero
ins <- paste0(ins, "[46+.>]")
# print: "!"
ins <- paste0(ins, "33+.")

bfkr_interpret(ins)