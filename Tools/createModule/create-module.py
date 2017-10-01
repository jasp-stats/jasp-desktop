import json
import os


# Read the modules file
with open('../../Resources/modules.json', 'r') as f:
    modules = json.load(f)

existing_modules = []

for _, dirs, _ in os.walk("../../JASP-Desktop/analysisforms", topdown=False):
    for name in dirs:
        existing_modules.append(name)

print('existing modules = ', existing_modules)

for module in modules:
    module_name = module['name'].replace(' ', '')
    module_dir = '../../JASP-Desktop/analysisforms/{0}'.format(module_name)

    if not os.path.exists(module_dir):
        print(module_name)
        os.makedirs(module_dir)

        # Create ribbon
        # {0, 1, 2} -> {SUMMARYSTATISTICS, SummaryStatistics, summarystatistics}


    # if module['name'].replace(' ', '') not in existing_modules:
    #     print(module['name'].replace(' ', ''))
        # Create



# print(existing_modules)
# find new modules
# for module in modules:
# 	if

# print(modules)
