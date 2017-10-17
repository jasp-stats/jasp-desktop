import json
import os


current_path = os.path.dirname(__file__)
ribbon_button_replacement_text = '///// Ribbon Buttons and Menu'
additional_headers = '///// additional Headers'
add_buttons = '<!-- Add buttons -->'
menu_selected_slot = '<!-- menuItemSelectedSlot -->'


def create_module_ribbon(module, ribbon):
    ''' Create ribbon files for the new module- .ui, .h, .cpp '''
    print('- {0} Ribbon'.format(module))
    ribbon_path = current_path + '/../../JASP-Desktop/ribbons/'
    # Read ribbon.h template
    with open(current_path + '/templates/ribbon.h', 'r') as f:
        ribbon_header = f.read()
    # {0, 1, 2} -> {SUMMARYSTATISTICS, SummaryStatistics, summarystatistics}
    ribbon_header = ribbon_header.format(module.upper(), module, module.lower())

    # Create ribbon.h
    ribbon_filename = 'ribbon{0}'.format(module.lower())
    with open(ribbon_path + ribbon_filename + '.h', 'w+') as f:
        f.write(ribbon_header)
    print('    Created Ribbon Header')

    # Read ribbon.cpp template
    with open(current_path + '/templates/ribbon.cpp', 'r') as f:
        ribbon_source = f.read()
    ribbon_source = ribbon_source.format(module.upper(), module, module.lower())

    # Create ribbon buttons
    ribbon_buttons = [analysis['name'] for analysis in ribbon]
    menu_header_required = False

    button_text = ''
    for obj in ribbon:
        button_name = obj['name']
        # FIXME: Write more general statements, use regex
        # FIXME: Handle duplicate ribbon and analyses names
        button_name = button_name.replace('-', '')
        button_name = button_name.replace(' ', '')
        button_text += '\taddRibbonButton(ui->{0});\n'.format('button' + button_name)
        # dataSetRequired
        dataSetRequired = obj.get('dataSetRequired')
        if dataSetRequired is not None and (not dataSetRequired):
            button_text += '\tui->{0}->setDataSetNotNeeded();\n'.format('button' + button_name)

        button_text += '\n'
        # Button menu - analyses
        analyses = obj.get('analyses')
        if analyses is not None:
            menu_header_required = True
            button_text += '\tQMenu *menu{0} = new QMenu(this);\n'.format(button_name);
            # Create Menu
            for analysis in analyses:
                # FIXME: Make this more general. Use regex.
                analysis_name = analysis.replace('-', '')
                analysis_name = analysis_name.replace(' ', '')
                button_text += '\tmenu{0}->addAction(QString("{1}"), this, SLOT(itemSelected()))->setObjectName("{2}");\n'.format(button_name, analysis, analysis_name)

            # Set Menu
            button_text += '\tui->{0}->setMenu({1});\n\n'.format('button' + button_name, 'menu' + button_name)

    button_text += (ribbon_button_replacement_text + '\n')
    ribbon_source = ribbon_source.replace(ribbon_button_replacement_text, ('\n' + button_text))

    if menu_header_required:
        ribbon_source = ribbon_source.replace(additional_headers, '#include <QMenu>\n{0}'.format(additional_headers))

    # Create ribbon.cpp
    with open(ribbon_path + ribbon_filename + '.cpp', 'w+') as f:
        f.write(ribbon_source)
    print('    Created Ribbon Source')

    # Read ribbon.ui template
    with open(current_path + '/templates/ribbon.ui', 'r') as f:
        ribbon_ui = f.read()
    # {0, 1, 2} -> {SUMMARYSTATISTICS, SummaryStatistics, summarystatistics}
    ribbon_ui = ribbon_ui.format(ribbon_name=module)

    # Read ribbon_button.ui template
    with open(current_path + '/templates/ribbon_button.ui', 'r') as f:
        ribbon_button = f.read()

    button_ui = ""
    column_number = 0
    for obj in ribbon:
        button_name = obj['name']
        # FIXME: Write more general statements, use regex
        # FIXME: Handle duplicate ribbon and analyses names
        button_name = button_name.replace('-', '')
        button_name = button_name.replace(' ', '')
        button_ui += ribbon_button.format(column_number=str(column_number), button_name=button_name,
                                          button_text=obj['name'])
        button_ui += '\n'
        column_number += 1

    ribbon_ui = ribbon_ui.replace(add_buttons, button_ui)

    if menu_header_required:
        ribbon_ui = ribbon_ui.replace(menu_selected_slot, '  <slot>menuItemSelected()</slot>')

    # Create ribbon.ui
    with open(ribbon_path + ribbon_filename + '.ui', 'w+') as f:
        f.write(ribbon_ui)
    print('    Created Ribbon Ui')


def create_layout_files(module, ribbon):
    ''' Create layout files for each analysis '''
    print('- Layout Files')

    analysis_path = current_path + '/../../JASP-Desktop/analysisforms/{0}/'.format(module)

    # Read layout.h template
    with open(current_path + '/templates/layout.h', 'r') as f:
        layout_header = f.read()
    # Read layout.cpp template
    with open(current_path + '/templates/layout.cpp', 'r') as f:
        layout_source = f.read()
    # Read layout.ui template
    with open(current_path + '/templates/layout.ui', 'r') as f:
        layout_ui = f.read()

    for obj in ribbon:
        analyses = obj.get('analyses')
        if analyses is None:
            analyses = [obj['name']]

        for analysis in analyses:
            # FIXME: Write more general statements, use regex
            # FIXME: Handle duplicate ribbon and analyses names
            analysis_name = analysis.replace('-', '')
            analysis_name = analysis_name.replace(' ', '')
            analysis_name = module + analysis_name + 'Form'

            analysis_header = layout_header.format(analysis_name.upper(), analysis_name, analysis_name.lower())
            analysis_source = layout_source.format(analysis_name.upper(), analysis_name, analysis_name.lower())
            analysis_ui = layout_ui.format(analysis_name.upper(), analysis_name, analysis_name.lower())

            # Create ribbon.cpp
            with open(analysis_path + analysis_name.lower() + '.h', 'w+') as f:
                f.write(analysis_header)
            # Create ribbon.cpp
            with open(analysis_path + analysis_name.lower() + '.cpp', 'w+') as f:
                f.write(analysis_source)
            # Create ribbon.cpp
            with open(analysis_path + analysis_name.lower() + '.ui', 'w+') as f:
                f.write(analysis_ui)

            print('    Created {0} Files'.format(analysis))

def modify_mainwindow(module, analyses):
    pass


def create_resource_files(module, ribbon):
    ''' Create options resource files for each analysis '''
    print('- Resrouce Files')

    resource_path = current_path + '/../../Resources/Library/'

    for obj in ribbon:
        analyses = obj.get('analyses')
        if analyses is None:
            analyses = [obj['name']]

        for analysis in analyses:
            # FIXME: Write more general statements, use regex
            # FIXME: Handle duplicate ribbon and analyses names
            analysis_name = analysis.replace('-', '')
            analysis_name = analysis_name.replace(' ', '')
            analysis_name = module + analysis_name

            content = '{{\n\t"name": "{0}",\n\t"autorun": true,\n\t"version": "1.0",\n\t"options": []\n}}'.format(analysis_name)

            # Create resource file
            with open(resource_path + analysis_name + '.json', 'w+') as f:
                f.write(content)

            print('    Created {0} resource file'.format(analysis))


def create_analyses_files(module, ribbon):
    ''' Create Analyses files '''
    print('- Analyses Files')

    analysis_path = current_path + '/../../JASP-Engine/JASP/R/'

    # Read analysis.R template
    with open(current_path + '/templates/analysis.R', 'r') as f:
        analysis_source = f.read()

    for obj in ribbon:
        analyses = obj.get('analyses')
        if analyses is None:
            analyses = [obj['name']]

        for analysis in analyses:
            # FIXME: Write more general statements, use regex
            # FIXME: Handle duplicate ribbon and analyses names
            analysis_name = analysis.replace('-', '')
            analysis_name = analysis_name.replace(' ', '')
            analysis_name = module + analysis_name

            analysis_source = analysis_source.format(analysis_name, analysis)

            # Create resource file
            with open(analysis_path + analysis_name.lower() + '.R', 'w+') as f:
                f.write(analysis_source)

            print('    Created {0} analysis file'.format(analysis))


def create_new_module():
    try:
        # Read the modules file
        with open(current_path + '/../../Resources/modules.json', 'r') as f:
            modules = json.load(f)
    except Exception as e:
        print('Exception occured - {exception}.'.format(exception=str(e)))
        exit(1)

    # TODO: Check if module name is unique
    for module in modules:
        module_name = module['name'].replace(' ', '')
        module_dir = current_path + '/../../JASP-Desktop/analysisforms/{0}'.format(module_name)

        # Check if module exists or not
        # FIXME: This is only temporary.
        #        Find a better way to check module existence
        if not os.path.exists(module_dir):
            os.makedirs(module_dir)

            create_module_ribbon(module_name, module['ribbon'])
            create_layout_files(module_name, module['ribbon'])
            create_resource_files(module_name, module['ribbon'])
            create_analyses_files(module_name, module['ribbon'])
            modify_mainwindow(module_name, module['ribbon'])

if __name__ == '__main__':
    create_new_module()
