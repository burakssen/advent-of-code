import matplotlib.pyplot as plt
import json
import subprocess

data = subprocess.check_output("gh api -H \"Accept: application/vnd.github+json\" -H \"X-Github-Api-Version: 2022-11-28\" /repos/burakssen/AdventOfCode/languages", shell=True)
data = json.loads(data)

print(data)

data.pop('CMake', None)
data.pop('Makefile', None)

scripts_line_counts = 0
with open('language_dist.py') as f:
    scripts_line_counts += len(f.readlines())

with open('download_inputs.py') as f:
    scripts_line_counts += len(f.readlines())

data['Python'] -= scripts_line_counts

plt.figure(dpi=1200)

fig, ax = plt.subplots()
ax.pie(data.values(), labels=data.keys(), autopct='%1.1f%%')
plt.title('AdventOfCode Used Language Distribution', fontsize=20, color='red')
plt.savefig('../language_distribution.svg')

