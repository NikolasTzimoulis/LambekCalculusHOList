theorem_database = "theorem_database_v1.1.textpb"
definition_types = set([])

for line in open(theorem_database, 'r'):
    if line.strip().startswith("definition_type"):
        definition_types.add(line.strip())
        
print(definition_types)