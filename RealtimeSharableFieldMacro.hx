package macro;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.ExprTools;
import haxe.macro.PositionTools;

class RealtimeSharableFieldMacro
{
	static var pos:Position;
	static var hider:Null<Metadata> = null;

	public static macro function build(HideCoreProps:Bool = true):Array<Field>
	{
		pos = Context.currentPos();

		hider = null;
		if (HideCoreProps)
		{
			hider = [
				{
					name: ":noCompletion",
					pos: pos,
				},
			];
		}

		var localName = Context.getLocalClass().get().name;

		var Name:String = localName.charAt(0);
		var Path:String = localName;

		var field_data:Field = getSaveField(Name, Path);

		var local:Array<Field> = Context.getBuildFields();
		var fields:Array<Field> = [field_data].concat(formatFields(local));

		return fields;
	}

	private static function getStaticFuncExpr(fields:Array<Field>, funcName:String):Expr
	{
		var expr:Expr = null;

		var filter = fields.filter(field ->
		{
			return field.name == funcName;
		});
		if (filter.length > 0)
		{
            expr = macro $i{funcName};
		}

		return expr;
	}

	private static function formatFields(fields:Array<Field>):Array<Field>
	{
		var expr_GetCallback:Expr = getStaticFuncExpr(fields, 'getCallback');
		var expr_SetCallback:Expr = getStaticFuncExpr(fields, 'setCallback');

		var expr_DataField:Expr = macro $i{'___data'};
		var expr_Data_DataField:Expr = extractFieldExprfromExpr(expr_DataField, 'data');
		var expr_Flush_DataField:Expr = extractFieldExprfromExpr(expr_DataField, 'flush');

		var formattedFields:Array<Field> = [];

		for (field in fields)
		{
			var isfield:Bool = isFieldVariable(field);

			if (!isfield)
			{
                if (['getCallback', 'setCallback'].contains(field.name))
                {
                    var doc = 'Used for Callback.';
                    if (field.doc != null && field.doc.length > 0)
                    {
                        doc += '\n\n${field.doc}';
                    }
                    
                    field.doc = doc;
                }

				formattedFields.push(field);
				continue;
			}

			var fieldname:String = field.name;

			var firsttype:String = extractTypes(field, true);
			var fulltypes:String = extractTypes(field, false);

			var t:ComplexType = getComplexType(field);
			var e:Expr = getExpr(field);

			var min:Int = PositionTools.getInfos(field.meta[0].pos).min;

			var _ep = PositionTools.getInfos(e.pos);
			var max:Int = _ep.min;

			var _p = PositionTools.getInfos(field.pos);
			var p = PositionTools.make({file: _p.file, min: min, max: max - 4 - firsttype.length});

			var document:String = '$fieldname:$fulltypes (default: ${ExprTools.toString(e)})';
			if (field.doc != null)
			{
				document += '\n\n${field.doc}';
			}

			var expr_fieldofdata:Expr = {
				expr: EField(expr_Data_DataField, fieldname),
				pos: pos,
			};

			var expr_resetfieldandflush:Expr = {
				expr: EBlock([
					macro $e{expr_fieldofdata} = $e{e},
					{
						expr: ECall(expr_Flush_DataField, []),
						pos: pos,
					},
				]),
				pos: pos,
			};

			var field_property:Field = {
				name: fieldname,
				pos: p,
				meta: [],
				kind: FProp('get', 'set', t, null),
				access: [APublic, AStatic],
				doc: document,
			};

			var field_getter:Field = makeGetter(expr_fieldofdata, fieldname, t, expr_resetfieldandflush, expr_GetCallback);
			var field_setter:Field = makeSetter(expr_fieldofdata, fieldname, t, expr_Flush_DataField, expr_SetCallback);

			formattedFields = formattedFields.concat([field_property, field_getter, field_setter]);
		}

		return formattedFields;
	}

	private static function makeGetter(expr:Expr, fieldName:String, t:ComplexType, resetExpr:Expr, ?callbackExpr:Expr):Field
	{
		var expr_return:Expr = expr;
		if (callbackExpr != null)
		{
			expr_return = {
				expr: ECall(callbackExpr, [expr_return]),
				pos: pos,
			};
		}

		var expr_getter:Expr = {
			expr: EBlock([
				{
					expr: EIf({
						expr: EBinop(OpEq, expr, {
							expr: EConst(CIdent('null')),
							pos: pos,
						}),
						pos: pos,
					}, resetExpr, null),
					pos: pos,
				},
				{
					expr: EReturn(expr_return),
					pos: pos,
				},
			]),
			pos: pos,
		};

		var field_getter:Field = {
			name: "get_" + fieldName,
			access: [APrivate, AInline, AStatic],
			kind: FFun({
				expr: expr_getter,
				args: [],
				ret: t,
			}),
			pos: pos,
			meta: hider,
		};

		return field_getter;
	}

	private static function makeSetter(expr:Expr, fieldName:String, t:ComplexType, saveExpr:Expr, ?callbackExpr:Expr):Field
	{
		var expr_access:Expr = macro $i{'value'};
		if (callbackExpr != null)
		{
			expr_access = {
				expr: ECall(callbackExpr, [expr_access]),
				pos: pos,
			};
		}

		var expr_setter:Expr = {
			expr: EBlock([
				macro $e{expr} = $e{expr_access},
				{
					expr: ECall(saveExpr, []),
					pos: pos,
				},
				{
					expr: EReturn(expr),
					pos: pos,
				},
			]),
			pos: pos,
		};

		var field_setter:Field = {
			name: "set_" + fieldName,
			access: [APrivate, AInline, AStatic],
			kind: FieldType.FFun({
				expr: expr_setter,
				args: [
					{
						name: "value",
						type: t,
					},
				],
				ret: t,
			}),
			pos: pos,
			meta: hider,
		};

		return field_setter;
	}

	private static function getSaveField(Name:String, ?Path:String):Field
	{
		var typepath_flxsave:TypePath = {
			pack: ["flixel", "util"],
			name: "FlxSave",
		};

		var expr_Name:Expr = {
			expr: ExprDef.EConst(CString(Name, DoubleQuotes)),
			pos: pos,
		};

		var expr_Path:Expr = {
			expr: ExprDef.EConst(CString(Path, DoubleQuotes)),
			pos: pos,
		};

		var field_data:Field = {
			name: '___data',
			pos: pos,
			kind: FVar(TPath(typepath_flxsave), {
				expr: ExprDef.ENew(typepath_flxsave, [expr_Name, expr_Path]),
				pos: pos
			}),
			meta: hider,
			access: [Access.APrivate, Access.AStatic],
		};

		return field_data;
	}

	private static function extractFieldExprfromExpr(expr:Expr, field:String):Expr
	{
		return {expr: EField(expr, field), pos: expr.pos};
	}

	private static function isFieldVariable(field:Field):Bool
	{
		return switch (field.kind)
		{
			case FVar(t, e): field.meta != null && field.meta.length == 1 && field.meta[0].name == "field";
			default: false;
		}
	}

	private static function getComplexType(field:Field):ComplexType
	{
		return switch (field.kind)
		{
			case FVar(t, e): t;
			default: null;
		}
	}

	private static function getExpr(field:Field):Expr
	{
		return switch (field.kind)
		{
			case FVar(t, e): e;
			default: null;
		}
	}

	private static function extractTypes(field:Field, first:Bool = false):String
	{
		var type:Null<ComplexType> = getComplexType(field);
		return type == null ? '' : findTypes(type, first);
	}

	private static function findTypes(type:ComplexType, first:Bool = false):String
	{
		var str:String = '';

		switch (type)
		{
			case TPath(p):
				str += p.name;

				var params = p.params;
				if (first && params.length > 1)
				{
					str += '(';

					for (param in params)
					{
						switch (param)
						{
							case TPType(t):
								str += findTypes(t, first);
							default:
						}
					}

					str += ')';
				}
			default:
		}

		return str;
	}
}
