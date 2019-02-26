#ifndef JASPDOUBLEVALIDATOR_H
#define JASPDOUBLEVALIDATOR_H

#include <QDoubleValidator>


class JASPDoubleValidator : public QDoubleValidator
{
public:
	JASPDoubleValidator (QObject * parent = nullptr) : QDoubleValidator(parent) {}
    JASPDoubleValidator (double bottom, double top, int decimals, QObject * parent) :
    QDoubleValidator(bottom, top, decimals, parent) {}

    QValidator::State validate(QString & s, int & pos) const;
};

#endif // JASPDOUBLEVALIDATOR_H
