//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef BOUNDCONTROLTEXTAREA_H
#define BOUNDCONTROLTEXTAREA_H

#include "boundcontrolbase.h"

class TextAreaBase;

class BoundControlTextArea : public BoundControlBase
{
public:
	friend TextAreaBase;

	BoundControlTextArea(TextAreaBase* textArea);

	//keep the JsonFormat overload visible through this class's concrete pointers (the createJson()
	//override below would otherwise hide it):
	using BoundControlBase::createJson;

	bool					isJsonValid(const Json::Value& optionValue) const	override;
	Json::Value				createJson()								const	override;
	void					bindTo(const Json::Value& value)					override;
	void					resetBoundValue()									override;
	void					setBoundValue(const Json::Value& value, bool emitChange = true)	override;

	virtual	void			checkSyntax();
	virtual QString			rScriptDoneHandler(const QString &result)	{ throw std::runtime_error("runRScript done but handler not implemented!\nImplement an override for RScriptDoneHandler!\nResult was: " + result.toStdString()); };

protected:
	///< The encoder for the data this text area's form/host is bound to: desktop never uses the
	///< process-global ColumnEncoder (only meaningful inside an engine request), it asks the
	///< provider of the resolved VariableInfo (see JASPControl::resolvedVarInfo).
	ColumnEncoder			*	_encoder()							const;

	///< (textarea code encode): resolve every qualified column reference
	/// (surface form) in `code` and rewrite it into the REFERENCED filter's view token,
	///  so the encoded tokens that form encode under the right namespaces - the surface form
	///  must never reach R's parser, and the bare-name encode pass that runs afterwards must
	///  never mangle a reference's components into the CONTEXT namespace. Bare names are not
	///  touched here: the context encoder (the host's filter view; the captured D<ds>_F<f>
	///  prefix in the bridge, D1.7) stays their fallback namespace (§4). Comments and R string
	///  literals are never references (QualifiedColumnRef::scanText rules). A reference that
	///  does not resolve throws filterException with the typed ready-to-show message (the loud
	///  D7.5 doctrine, no tombstoning) - the textareas surface it through their checkSyntax
	///  error handling. `tokensFormed` (when given) receives the encoded tokens in order of
	///  appearance, for the callers' used-column bookkeeping.
	static QString				encodeQualifiedColumnRefs(const QString & code, QStringList * tokensFormed = nullptr);

	///< a TextArea commits its option on APPLY (typing alone never writes) and a
	/// re-initialisation deliberately restores the editor's draft text (TextAreaBase::
	/// _setInitialized) - so the widget text is NOT the option value and cannot be generated
	/// from. The last applied/bound value is this component's own typed state instead (every
	/// write path funnels through setBoundValue, and BoundControlBase::bindTo calls this very
	/// virtual), which is what both generators emit; while the mirror still exists it holds the
	/// same value. rlang/JAGS companions inherit the tracking and only replace createJson's shape.
	Json::Value				createEngineValue()										const	override;

	Json::Value					_appliedValue;

	TextAreaBase*				_textArea	= nullptr;
};

#endif // BOUNDCONTROLTEXTAREA_H
