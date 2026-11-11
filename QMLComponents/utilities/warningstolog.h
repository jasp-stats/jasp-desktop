#ifndef WARNINGSTOLOG_H
#define WARNINGSTOLOG_H

/// MessageForwarder::showWarning opens a QMessageBox, which a process without widgets (the syntax bridge) cannot do.
/// After this call the warnings are written to the log instead.
/// It lives in its own header because messageforwarder.h needs the headers of QtWidgets.
void sendWarningsToLog();

#endif // WARNINGSTOLOG_H
