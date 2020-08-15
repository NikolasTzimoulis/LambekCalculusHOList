theorem_database = "sequents_TEMP.txt"
theorem_number = 0
total_tokens = 0

for line in open(theorem_database, 'r'):
    if line.strip().startswith("conclusion:"):
        theorem_number += 1
        total_tokens += len(line.strip().split()) - 1
        
print(total_tokens, theorem_number, total_tokens / theorem_number)