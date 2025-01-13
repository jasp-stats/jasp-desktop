#ifndef DESKTOPCOMMUNICATOR_H
#define DESKTOPCOMMUNICATOR_H

#include <QObject>

///This class only exists to allow signal-slot connections to be made between certain classes in Desktop and in QMLComponents.
/// And to easily split that off when building for R -only
class DesktopCommunicator : public QObject
{
	Q_OBJECT
public:
	explicit DesktopCommunicator(QObject *parent = nullptr);
	
	static DesktopCommunicator * singleton();

	bool useNativeFileDialog();
	void setFilterFunctions(
		std::function<void(const std::string&)>						_builderFunc,
		std::function<void(const std::string&)>						_deleteFunc,
		std::function<void(const std::string&, const std::string&)>	_setRFilterFunc,
		std::function<bool(const std::string&)>						_checkForUpdateFunc,
		std::function<std::vector<bool>(const std::string&)>		_filteredFunc,
		std::function<int(const std::string&)>						_filteredRowCountFunc,
		std::function<bool(const std::string&)>						_filterExistFunc);
	void filterBuild(const std::string& name);
	void filterDelete(const std::string& name);
	void filterSetRScript(const std::string& name, const std::string& rFilter);
	bool filterCheckForUpdate(const std::string& name);
	std::vector<bool> filtered(const std::string& name);
	int filteredRowCount(const std::string& name);
	bool filterNameIsFree(const std::string& name);

signals:
	void currentJaspThemeChanged();
	void uiScaleChanged();
	void interfaceFontChanged();
	bool useNativeFileDialogSignal(); //< For internal use only, `bool useNativeFileDialog();` is what you want
	
private:
	static DesktopCommunicator * _singleton;

	std::function<void(const std::string&)>						_filterBuilderFunc;
	std::function<void(const std::string&)>						_filterDeleteFunc;
	std::function<void(const std::string&, const std::string&)>	_filterSetRFilterFunc;
	std::function<bool(const std::string&)>						_filterCheckForUpdateFunc;
	std::function<std::vector<bool>(const std::string&)>		_filterFilteredFunc;
	std::function<int(const std::string&)>						_filterFilteredRowCountFunc;
	std::function<bool(const std::string&)>						_filterNameExistFunc;
};

#endif // DESKTOPCOMMUNICATOR_H
