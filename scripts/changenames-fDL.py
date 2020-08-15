from collections import OrderedDict 

old_theorem_database = "theorem_database_TEMP.textpb"
new_theorem_database = "theorem_database_TEMP_changednames.textpb"
propertiesToChange = ["conclusion:", "type_name:", "abs_name:", "rep_name:", "definition_term:", "constants:"]
replacements = OrderedDict([
                (r"_mk_string", "hypermap"),
                (r"_dest_string", "tuple_hypermap"),
                (r"string", "hypermap"),
                (r"_mk_pform", "cc_v11"),
                (r"_dest_pform", "pair_of_cc_v11"),
                (r"pform", "cc_v11"),
                (r"_mk_nform", "mk_finite_product"),
                (r"_dest_nform", "dest_finite_product"),
                (r"nform", "finite_product"),
                (r"_mk_form", "loop"),
                (r"_dest_form", "tuple_loop"),
                (r"form", "loop"),
                (r"_mk_strF", "stable_sy"),
                (r"_dest_strF", "tuple_stable_sy"),
                (r"strF", "stable_sy"),
                (r"_mk_strG", "tri_sy"),
                (r"_dest_strG", "tuple_tri_sy"),
                (r"strG", "tri_sy"),
                (r"_mk_seq", "scs_v39"),
                (r"_dest_seq", "dest_scs_v39"),
                (r"seq", "scs_v39"),
                (r"||^", "halfatn4"),
                (r"||", "halfatn"),
                (r"nn", "plane_norm"),
                (r"pp", "plane"),
                (r"**^", "*:"),
                (r"**", "*."),
                (r"/.^", "**:"),
                (r"/.", "**."),
                (r"ff", "mark_term"),
                (r"gg", "TAGB"),
                (r"<>^", "twopow"),
                (r"<>", "ssqrt"),
                (r"/-^", "-:"),
                (r"/-", "-."),
                (r"##^", "+:"),
                (r"##", "+."),
                (r"\\-", "-|"),
                (r"\\-^", "+|"),
                (r"pAtom", "drop0"),
                (r"String", "nabs"),
                (r"\\.^", "**|"),
                (r"\\.", "*|"),
                (r"focf", "drop2"),
                (r"-->", "<=."),
                (r"focg", "drop3"),
                (r"nAtom", "drop1"),
                (r"|--", "RC")])

print(replacements)

outFile = open(new_theorem_database, 'w')

for line in open(old_theorem_database, 'r'):
    fixedLine = line
    for prop in propertiesToChange:
        if line.strip().startswith(prop):
            for old, new in replacements.items():
                fixedLine = fixedLine.replace(old, new)
    outFile.write(fixedLine)
