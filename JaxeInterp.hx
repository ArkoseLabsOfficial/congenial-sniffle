package jaxe;

import jaxe.JaxeExpr;
import Reflect;
import Type;

class JaxeInterp {
	public var scriptObject:Dynamic;
	public var extendedObject:Dynamic;
	public var scriptName:String = "Unknown";

	public var methods:Map<String, {args:Array<{name:String, type:String}>, body:JaxeExpr}> = new Map();
	public var vars:Map<String, Dynamic> = new Map();
	var imports:Map<String, Dynamic> = new Map();
	var usings:Array<Dynamic> = [];

	public function new() {}
	public function setVar(name:String, value:Dynamic) vars.set(name, value);

	public function callMethodName(name:String, args:Array<Dynamic>):Dynamic {
		if (!methods.exists(name)) return null;
		var m = methods.get(name);

		var oldVars = new Map<String, Dynamic>();
		for (i in 0...m.args.length) {
			var argName = m.args[i].name;
			if (vars.exists(argName)) oldVars.set(argName, vars.get(argName));
			vars.set(argName, i < args.length ? args[i] : null);
		}

		var res = null;
		try {
			res = eval(m.body);
		} catch(e:String) {
			if (e != "JAXE_BREAK" && e != "JAXE_CONTINUE") throw e;
		} catch(e:Dynamic) {
			trace("Jaxe Runtime Error in method [" + name + "]: " + e);
		}

		for (i in 0...m.args.length) {
			var argName = m.args[i].name;
			if (oldVars.exists(argName)) vars.set(argName, oldVars.get(argName));
			else vars.remove(argName);
		}
		return res;
	}

	public function execute(ast:Array<JaxeExpr>) { for (expr in ast) eval(expr); }

	public function eval(e:JaxeExpr):Dynamic {
		if (e == null) return null;
		switch (e) {
			case EPackage(pack): return null;
			case EImport(pack):
				var cls = Type.resolveClass(pack);
				if (cls == null) {
					var parts = pack.split(".");
					cls = Type.resolveClass(parts[parts.length - 1]);
				}
				var parts = pack.split(".");
				imports.set(parts[parts.length - 1], cls);
				return null;
			case EScriptImport(pathExpr):
				var path:String = eval(pathExpr);
				#if sys
				if (sys.FileSystem.exists(path)) {
					var code = sys.io.File.getContent(path);
					var subParser = new JaxeParser();
					var subAst = subParser.parse(code, path);
					this.execute(subAst); 
				} else throw 'Import Error: Script not found at $path';
				#end
				return null;
			case EUsing(pack):
				var cls = Type.resolveClass(pack);
				if (cls == null) {
					var parts = pack.split(".");
					cls = Type.resolveClass(parts[parts.length - 1]);
				}
				if (cls != null) usings.push(cls);
				return null;
			case EScriptUsing(pathExpr):
				var path:String = eval(pathExpr);
				#if sys
				if (sys.FileSystem.exists(path)) {
					var code = sys.io.File.getContent(path);
					var subParser = new JaxeParser();
					var subAst = subParser.parse(code, path);
					this.execute(subAst); 
					for (expr in subAst) {
						switch(expr) {
							case EClass(name, _, _): if (vars.exists(name)) usings.push(vars.get(name));
							default:
						}
					}
				} else throw 'Using Error: Script not found at $path';
				#end
				return null;
			case EClass(name, superclass, members):
				if (superclass != "") {
					var cls = Type.resolveClass(superclass);
					if (cls == null && imports.exists(superclass)) cls = imports.get(superclass);
					if (cls != null) {
						extendedObject = Type.createInstance(cls, []);
						try { Reflect.setProperty(extendedObject, "scriptInterp", this); } catch(e:Dynamic) {}
					}
				}
				var classObj:Dynamic = {}; vars.set(name, classObj);
				for (m in members) {
					switch (m) {
						case EMethod(mName, _, _, _, _, isStatic):
							if (isStatic) Reflect.setProperty(classObj, mName, Reflect.makeVarArgs(function(args:Array<Dynamic>) return callMethodName(mName, args)));
						case EVarDecl(vName, _, init, _, isStatic):
							if (isStatic) Reflect.setProperty(classObj, vName, init != null ? eval(init) : null);
						default:
					}
					eval(m);
				}

				if (methods.exists(name)) callMethodName(name, []);
				if (methods.exists("new")) callMethodName("new", []);
				if (methods.exists("main")) callMethodName("main", []);

				return null;
			case EVarDecl(name, type, init, isPriv, isStatic):
				vars.set(name, init != null ? eval(init) : null); return null;
			case EMethod(name, type, args, body, isOverride, isStatic):
				var hasSuper = false;
				if (extendedObject != null && (Reflect.hasField(extendedObject, name) || Reflect.getProperty(extendedObject, name) != null)) hasSuper = true;
				if (scriptObject != null && (Reflect.hasField(scriptObject, name) || Reflect.getProperty(scriptObject, name) != null)) hasSuper = true;
				if (isOverride && !hasSuper) throw 'Method \'$name\' is marked @Override but no super method exists.';

				methods.set(name, {args: args, body: body});
				if (hasSuper) {
					var scriptFunc = Reflect.makeVarArgs(function(funcArgs:Array<Dynamic>) return callMethodName(name, funcArgs));
					if (extendedObject != null && (Reflect.hasField(extendedObject, name) || Reflect.getProperty(extendedObject, name) != null)) {
						try { Reflect.setProperty(extendedObject, name, scriptFunc); } catch(e:Dynamic) {}
					}
					if (scriptObject != null && (Reflect.hasField(scriptObject, name) || Reflect.getProperty(scriptObject, name) != null)) {
						try { Reflect.setProperty(scriptObject, name, scriptFunc); } catch(e:Dynamic) {}
					}
				}
				return null;

			case ENew(cls, args):
				var tCls = Type.resolveClass(cls);
				if (tCls == null && imports.exists(cls)) tCls = imports.get(cls);
				if (tCls == null) {
					if (vars.exists(cls) || cls == scriptName.split(".")[0]) {
						if (extendedObject != null) return extendedObject;
					}
					throw 'Class $cls not found.';
				}
				return Type.createInstance(tCls, args.map(eval));
			case ENewArray(cls, size):
				var s:Int = Std.int(cast eval(size));
				var arr = new Array<Any>();
				for(i in 0...s) arr.push(null);
				return arr;
			case EPostfix(expr, op):
				var orig:Float = eval(expr);
				var newVal = (op == "++") ? orig + 1 : orig - 1;
				switch(expr) {
					case EIdent(id):
						if (vars.exists(id)) vars.set(id, newVal);
						else if (extendedObject != null && (Reflect.hasField(extendedObject, id) || Reflect.getProperty(extendedObject, id) != null)) Reflect.setProperty(extendedObject, id, newVal);
						else if (scriptObject != null && (Reflect.hasField(scriptObject, id) || Reflect.getProperty(scriptObject, id) != null)) Reflect.setProperty(scriptObject, id, newVal);
					case EArrayAccess(arr, idx): 
						var tArr:Dynamic = eval(arr);
						try { tArr[Std.int(cast eval(idx))] = newVal; } catch(e:Dynamic) { Reflect.setProperty(tArr, Std.string(eval(idx)), newVal); }
					case EField(obj, f): Reflect.setProperty(eval(obj), f, newVal);
					default:
				}
				return orig;

			case EIf(cond, e1, e2): if (eval(cond) == true) return eval(e1); else if (e2 != null) return eval(e2); return null;
			case EWhile(cond, body): while (eval(cond) == true) { try { eval(body); } catch (e:String) { if (e == "JAXE_BREAK") break; if (e == "JAXE_CONTINUE") continue; throw e; } } return null;
			case EFor(init, cond, inc, body):
				if (init != null) eval(init);
				while (cond == null || eval(cond) == true) {
					try { eval(body); } catch (e:String) { if (e == "JAXE_BREAK") break; if (e == "JAXE_CONTINUE") { if (inc != null) eval(inc); continue; } throw e; }
					if (inc != null) eval(inc);
				}
				return null;
			case EForEach(varName, iterable, body):
				var iter:Array<Dynamic> = cast eval(iterable);
				if (iter != null) {
					for (item in iter) {
						vars.set(varName, item);
						try { eval(body); } catch (e:String) { if (e == "JAXE_BREAK") break; if (e == "JAXE_CONTINUE") continue; throw e; }
					}
				}
				return null;
			case ESwitch(cond, cases, def):
				var cVal:Dynamic = eval(cond); var matched = false;
				for (c in cases) {
					if (eval(c.val) == cVal) {
						matched = true;
						try { for(b in c.body) eval(b); } catch (e:String) { if (e == "JAXE_BREAK") break; else throw e; }
						break;
					}
				}
				if (!matched && def != null) { try { for(b in def) eval(b); } catch (e:String) { if (e == "JAXE_BREAK") {} else throw e; } }
				return null;
			case EBreak: throw "JAXE_BREAK"; case EContinue: throw "JAXE_CONTINUE";
			case EBlock(exprs): var res:Dynamic = null; for (ex in exprs) res = eval(ex); return res;

			case EArrayDecl(exprs): return exprs.map(eval);
			case EArrayAccess(target, index): 
				var tArr:Dynamic = eval(target);
				var idx:Dynamic = eval(index);
				if (tArr != null) {
					try {
						var res = tArr[Std.int(cast idx)];
						if (res != null) return res;
					} catch(e:Dynamic) {}
					return Reflect.getProperty(tArr, Std.string(idx));
				}
				return null;
			case EUnop(op, expr): if (op == "!") return !eval(expr); return null;

			case EAssign(target, val):
				var value = eval(val);
				switch (target) {
					case EIdent(id):
						if (vars.exists(id)) vars.set(id, value);
						else if (extendedObject != null && (Reflect.hasField(extendedObject, id) || Reflect.getProperty(extendedObject, id) != null)) Reflect.setProperty(extendedObject, id, value);
						else if (scriptObject != null && (Reflect.hasField(scriptObject, id) || Reflect.getProperty(scriptObject, id) != null)) Reflect.setProperty(scriptObject, id, value);
						else vars.set(id, value);
					case EArrayAccess(arrTarget, idxTarget):
						var tArr:Dynamic = eval(arrTarget);
						var idx:Dynamic = eval(idxTarget);
						if (tArr != null) {
							try { tArr[Std.int(cast idx)] = value; } catch(e:Dynamic) { Reflect.setProperty(tArr, Std.string(idx), value); }
						}
					case EField(EThis, id):
						if (extendedObject != null && (Reflect.hasField(extendedObject, id) || Reflect.getProperty(extendedObject, id) != null)) Reflect.setProperty(extendedObject, id, value);
						else vars.set(id, value);
					case EField(objExpr, field): 
						var obj = eval(objExpr);
						if (obj != null) Reflect.setProperty(obj, field, value);
					default:
				}
				return value;
			case EField(target, field):
				if (target == ESuper) return Reflect.getProperty(extendedObject != null ? extendedObject : scriptObject, field);
				if (target == EThis) {
					if (extendedObject != null && (Reflect.hasField(extendedObject, field) || Reflect.getProperty(extendedObject, field) != null)) return Reflect.getProperty(extendedObject, field);
					if (vars.exists(field)) return vars.get(field); 
				}
				return Reflect.getProperty(eval(target), field);
			case EIdent(id):
				if (vars.exists(id)) return vars.get(id);
				if (extendedObject != null && (Reflect.hasField(extendedObject, id) || Reflect.getProperty(extendedObject, id) != null)) return Reflect.getProperty(extendedObject, id);
				if (scriptObject != null && (Reflect.hasField(scriptObject, id) || Reflect.getProperty(scriptObject, id) != null)) return Reflect.getProperty(scriptObject, id);
				if (imports.exists(id)) return imports.get(id);
				return null;
			case ECall(target, args):
				var evalArgs = args.map(eval);
				if (target.match(EIdent("trace"))) { trace(evalArgs[0]); return null; }

				switch (target) {
					case EIdent(methodName):
						if (methods.exists(methodName)) return callMethodName(methodName, evalArgs);
						if (extendedObject != null && (Reflect.hasField(extendedObject, methodName) || Reflect.getProperty(extendedObject, methodName) != null)) return Reflect.callMethod(extendedObject, Reflect.getProperty(extendedObject, methodName), evalArgs);
						if (scriptObject != null && (Reflect.hasField(scriptObject, methodName) || Reflect.getProperty(scriptObject, methodName) != null)) return Reflect.callMethod(scriptObject, Reflect.getProperty(scriptObject, methodName), evalArgs);
						return null;
					case EField(EThis, methodName):
						if (extendedObject != null && (Reflect.hasField(extendedObject, methodName) || Reflect.getProperty(extendedObject, methodName) != null)) return Reflect.callMethod(extendedObject, Reflect.getProperty(extendedObject, methodName), evalArgs);
						if (methods.exists(methodName)) return callMethodName(methodName, evalArgs);
						return null;
					case EField(ESuper, methodName):
						var targetObj = extendedObject != null ? extendedObject : scriptObject;
						return Reflect.callMethod(targetObj, Reflect.getProperty(targetObj, methodName), evalArgs);
					case EField(objExpr, methodName):
						var obj = eval(objExpr);
						var func = Reflect.getProperty(obj, methodName);
						if (func != null) return Reflect.callMethod(obj, func, evalArgs);
						for (u in usings) {
							var staticFunc = Reflect.getProperty(u, methodName);
							if (staticFunc != null) {
								evalArgs.unshift(obj);
								return Reflect.callMethod(u, staticFunc, evalArgs);
							}
						}
						throw 'Method $methodName not found on object.';
					default:
						var func = eval(target);
						if (Reflect.isFunction(func)) return Reflect.callMethod(null, func, evalArgs);
						return null;
				}
			case EString(s): return s; case EInt(i): return i; case EFloat(f): return f; case EBool(b): return b;
			case EBinop(op, e1, e2):
				var v1:Dynamic = eval(e1); var v2:Dynamic = eval(e2);
				switch (op) { 
					case "+": return v1 + v2; case "-": return v1 - v2; case "*": return v1 * v2; case "/": return v1 / v2; case "%": return v1 % v2;
					case "==": return v1 == v2; case "!=": return v1 != v2;
					case ">": return v1 > v2; case "<": return v1 < v2; case ">=": return v1 >= v2; case "<=": return v1 <= v2;
					case "&&": return v1 && v2; case "||": return v1 || v2;
					default: return null;
				}
			default: return null;
		}
		return null;
	}
}
