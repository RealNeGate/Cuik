import os
import re
import subprocess

tests_done = 0
tests_passed = 0

pattern = re.compile(r'\x1b[^m]*m')

def read_file(path):
	file = open(path, mode='r')
	all_of_it = file.read()
	file.close()
	return all_of_it

def handle_file(filepath):
	global pattern
	global tests_passed
	global tests_done
	tests_done += 1

	result = subprocess.run(['cuik', filepath, '-xe'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)

	# verify the source
	gold = read_file(filepath + ".gold")
	gold_lines = gold.splitlines(keepends=False)

	result_lines = (pattern.sub('', result.stdout)).splitlines(keepends=False)

	gold_line_len = len(gold_lines)
	result_line_len = len(result_lines)
	if gold_line_len != result_line_len:
		print("Line count don't match (expected " + gold_line_len + ", got " + result_line_len)

	min_len = min(gold_line_len, result_line_len)
	error_count = 0
	for i in range(min_len):
		if gold_lines[i].strip() != result_lines[i].strip():
			print(f"Line {i} does not match")
			print("GOT:\n" + result_lines[i])
			print("EXPECTED:\n" + gold_lines[i])
			error_count += 1

	if error_count > 0:
		print("Compile result does not match gold for: " + filepath)
		return

	print("Success! " + filepath)
	tests_passed += 1

def handle_dir(filepath):
	for file in os.listdir(filepath):
		file = filepath + file
		if os.path.isdir(file):
			handle_dir(file)
		elif os.path.isfile(file) and file.endswith(".c"):
			handle_file(file)

handle_dir("./")
print(f"Passed {tests_passed} of {tests_done}!")
