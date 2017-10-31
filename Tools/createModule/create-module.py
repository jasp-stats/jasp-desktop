import json
import os


current_path = os.path.dirname(__file__)
ribbon_button_replacement_text = '///// Ribbon Buttons and Menu'
additional_headers = '///// additional Headers'
add_buttons = '<!-- Add buttons -->'
menu_selected_slot = '<!-- menuItemSelectedSlot -->'
analyses_headers_replacement = '///// 1-analyses headers'
ribbon_datasetloaded_replacement = '///// 2-ribbon setDataSetLoaded'
ribbon_itemselected_replacement = '///// 3-connect ribbon itemSelected'
if_else_ladder_replacement = '///// 4-analysis if-else ladder'
tab_changed_ribbon_number = '///// 5-ribbon tab number:'
ribbon_update_status_replacement = '///// 6-ribbon updateMenuEnabledDisabledStatus'
ribbon_update_ui_replacement = '///// 7-ribbon updateUIFromOptions'
ribbon_widget_replacement = '<!-- Add ribbon widget page -->'
customwidget_definition_replacement = '<!-- Add customwidget definition -->'


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

    button_ui = ''
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


def modify_mainwindow(module, ribbon):
    ''' Add analyses to mainwindow '''
    # 1. Analyses headers
    # 2. constructor - ribbon datasetloaded false
    # 3. constructor - connect ribbon to itemSelected slot
    # 4. Add analyses to if-else ladder
    # 5. Add ribbon number to tabChanged
    # 6. update menu (enabled) if dataset required by module
    # 7. update ribbon from ui options

    # 8. Add ribbon page to mainwindow ui file

    mainwindow_path = current_path + '/../../JASP-Desktop/mainwindow.'
    module_name = module.replace(' ', '')
    analyses_headers = ''
    analyses_if_else = ''
    mainwindow_source = ''
    with open(mainwindow_path + 'cpp', 'r') as f:
        mainwindow_source = f.read()

    for obj in ribbon:
        analyses = obj.get('analyses')
        if analyses is None:
            analyses = [obj['name']]

        for analysis in analyses:
            # FIXME: Write more general statements, use regex
            # FIXME: Handle duplicate ribbon and analyses names
            analysis_name = analysis.replace('-', '')
            analysis_name = analysis_name.replace(' ', '')
            analysis_name = module_name + analysis_name
            analyses_if_else += ('\telse if (name == "{0}")\n\t\tform = new {1}(contentArea);\n'.format(analysis_name, (analysis_name + 'Form')))

            analysis_name += 'Form'
            analyses_headers += ('#include "analysisforms/{0}/{1}.h"\n'.format(module_name, analysis_name.lower()))

    analyses_headers += ('\n' + analyses_headers_replacement)
    analyses_if_else += if_else_ladder_replacement

    ribbon_datasetloaded = '\tui->ribbon{0}->setDataSetLoaded(false);\n'.format(module_name)
    ribbon_datasetloaded += ribbon_datasetloaded_replacement

    ribbon_itemselected = '\tconnect(ui->ribbon{0}, SIGNAL(itemSelected(QString)), this, SLOT(itemSelected(QString)));\n'.format(module_name)
    ribbon_itemselected += ribbon_itemselected_replacement

    for line in mainwindow_source.splitlines():
        if tab_changed_ribbon_number in line:
            next_tab_number = line.split()[-1]
            replacement_text = '\t\telse if(currentActiveTab == "{0}")\n\t\t{{\n\t\t\tui->ribbon->setCurrentIndex({1});\n\t\t}}\n'.format(module, next_tab_number)
            replacement_text += (tab_changed_ribbon_number + ' {0}'.format(str(int(next_tab_number) + 1)))

            mainwindow_source = mainwindow_source.replace(line, replacement_text)
            break

    mainwindow_source = mainwindow_source.replace(analyses_headers_replacement, analyses_headers)
    mainwindow_source = mainwindow_source.replace(ribbon_datasetloaded_replacement, ribbon_datasetloaded)
    mainwindow_source = mainwindow_source.replace(ribbon_itemselected_replacement, ribbon_itemselected)
    mainwindow_source = mainwindow_source.replace(if_else_ladder_replacement, analyses_if_else)

    ribbon_update_status = '\tui->ribbon{0}->setDataSetLoaded(loaded);\n'.format(module_name)
    ribbon_update_status += ribbon_update_status_replacement

    mainwindow_source = mainwindow_source.replace(ribbon_update_status_replacement, ribbon_update_status)

    # ribbon_update_ui_replacement
    ribbon_update_ui = '\n\tQVariant variant_{name} = _settings.value("toolboxes/{name}", false);\n'.format(name=module_name)
    ribbon_update_ui += ('\tif (variant_{name}.canConvert(QVariant::Bool) && variant_{name}.toBool())\n'.format(name=module_name))
    ribbon_update_ui += ('\t\tui->tabBar->addTab("{module}");\n'.format(module=module))
    ribbon_update_ui += ('\telse\n\t\tui->tabBar->removeTab("{module}");\n'.format(module=module))
    ribbon_update_ui += ribbon_update_ui_replacement

    mainwindow_source = mainwindow_source.replace(ribbon_update_ui_replacement, ribbon_update_ui)

    # Write to mainwindow.cpp
    with open(mainwindow_path + 'cpp', 'w') as f:
        f.write(mainwindow_source)

    # Modify mainwindow ui
    mainwindow_ui = ''
    with open(mainwindow_path + 'ui', 'r') as f:
        mainwindow_ui = f.read()

    replacement_text = '          <widget class="Ribbon{name}" name="ribbon{name}"/>\n'.format(name=module_name)
    replacement_text += ribbon_widget_replacement
    mainwindow_ui = mainwindow_ui.replace(ribbon_widget_replacement, replacement_text)

    replacement_text = '  <customwidget>\n   <class>Ribbon{name}</class>\n   <extends>QWidget</extends>\n'.format(name=module_name)
    replacement_text += '   <header>ribbons/ribbon{header}.h</header>\n   <container>1</container>\n  </customwidget>\n'.format(header=module_name.lower())
    replacement_text += customwidget_definition_replacement
    mainwindow_ui = mainwindow_ui.replace(customwidget_definition_replacement, replacement_text)

    # Write to mainwindow.ui
    with open(mainwindow_path + 'ui', 'w') as f:
        f.write(mainwindow_ui)

    print('- Modified mainwindow')


def modify_tabbar(module, ribbon):
    ''' Add analyses to tabbar.cpp '''
    pass


def create_resource_files(module, ribbon):
    ''' Create options resource files for each analysis '''
    print('- Resource Files')

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

            analysis_source_temp = analysis_source.format(analysis_name, analysis)

            # Create resource file
            with open(analysis_path + analysis_name.lower() + '.R', 'w+') as f:
                f.write(analysis_source_temp)

            print('    Created {0} analysis file'.format(analysis))


def create_pri_file(module, ribbon):
    ''' Create pri file to include the layout and ribbon files '''
    print('- pri File')

    pri_file_path = current_path + '/../../JASP-Desktop/analysisforms/{0}/'.format(module)

    file_types = {
        'SOURCES': 'cpp',
        'HEADERS': 'h',
        'FORMS': 'ui'
    }

    content = ''
    for t in file_types.keys():
        content += (t + ' += \\\n')
        # FIXME: Ribbon path is hardcoded
        content += ('    $$PWD/../../ribbons/ribbon{0}.{1} \\\n'.format(module.lower(), file_types[t]))

        for j in range(len(ribbon)):
            analyses = ribbon[j].get('analyses')
            if analyses is None:
                analyses = [ribbon[j]['name']]

            for idx in range(0, len(analyses)):
                # FIXME: Write more general statements, use regex
                # FIXME: Handle duplicate ribbon and analyses names
                analysis_name = analyses[idx].replace('-', '')
                analysis_name = analysis_name.replace(' ', '')
                analysis_name = module + analysis_name + 'Form'
                content += ('    $$PWD/{0}.{1}'.format(analysis_name.lower(), file_types[t]))
                if idx == len(analyses) - 1 and j == len(ribbon) - 1:
                    content += ('\n')
                else:
                    content += (' \\\n')

        content += '\n'

    # Create module.pri
    with open(pri_file_path + module + '.pri', 'w+') as f:
        f.write(content)


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
            create_pri_file(module_name, module['ribbon'])
            modify_mainwindow(module['name'], module['ribbon'])
            modify_tabbar(module['name'], module['ribbon'])

if __name__ == '__main__':
    create_new_module()
