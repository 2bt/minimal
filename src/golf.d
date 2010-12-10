module src.golf;

import std.stdio;
import std.string;
import std.math;

import src.golfobject;

class Golf {
	struct Value {
		void delegate(Golf golf)	func;
		GolfObject					go;
	}

	GolfObject[]	stack;
	int[]			brackets;
	Value[string]	table;
	void*			userdata = null;

	GolfObject pop() {
		foreach_reverse(ref i; brackets) {
			if(i < stack.length) break;
			i--;
		}
		if(!stack.length) throw new Exception("popping from empty stack");
		GolfObject go = stack[$ - 1];
		stack = stack[0 .. $ - 1];
		return go;
	}
	void exec(string code) {
		int pos = 0;
		bool assign = false;
		void parseComment() {
			while(pos < code.length) {
				pos++;
				if(code[pos - 1] == '\n') return;
			}
		}
		void parseString() {
			while(pos < code.length) {
				pos++;
				if(code[pos - 1] == '\\') {
					if(pos == code.length) return;
					pos++;
				}
				else if(code[pos - 1] == '"') return;
			}
		}
		int parseBlock() {
			while(pos < code.length) {
				char c = code[pos++];
				if(c == '#') parseComment;
				else if(c == '"') parseString;
				else if(c == '}') return pos - 1;
				else if(c == '{') parseBlock;
			}
			return pos;
		}

		while(pos < code.length) {
			char c = code[pos++];
			if(iswhite(c)) { }
			else if(c == '#') parseComment;
			else if(c == ':') assign = true;
			else if(c == '{') {
				int old = pos;
				int p = parseBlock;
				stack ~= new GolfObject(BLOCK, code[old .. p]);
			}
			else if(c == '"') {
				string s;
				while(pos < code.length) {
					if(code[pos] == '\\') {
						pos++;
						if(pos == code.length) break;
					}
					else if(code[pos] == '"') { pos++; break; }
					s ~= code[pos++];
				}
				stack ~= new GolfObject(STRING, s);
			}
			else if((c >= '0' && c <= '9') || 
					(pos < code.length && c == '-' && code[pos] >= '0' && code[pos] <= '9')) {
				int old = pos - 1;
				while(pos < code.length) {
					c = code[pos];
					if(!(c >= '0' && c <= '9')) break;
					pos++;
				}
				stack ~= new GolfObject(atoi(code[old .. pos]));
			}
			else {	// name
				int old = pos - 1;
				if(c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_') {
					while(pos < code.length) {
						c = code[pos];
						if(!(c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9')) break;
						pos++;
					}
				}
				string name = code[old .. pos];
				if(assign) {
					assign = false;
					GolfObject go = pop;
					stack ~= go;
					table[name] = Value(null, go);
				}
				else {	// call
					if(!(name in table))
						throw new Exception(format("no value named \"%s\"", name));
					auto value = table[name];
					if(value.func) value.func(this);
					else {
						if(value.go.type == BLOCK)
							exec(value.go.stringToString);
						else
							stack ~= value.go;
					}
				}
			}
		}
	}

	this() {
		// built-ins
		table["n"] = Value(null, new GolfObject(STRING, "\n"));
		table["h"] = Value(null, new GolfObject(STRING, "Hello World"));
		table["["] = Value((Golf golf) { golf.brackets ~= golf.stack.length; });
		table["]"] = Value((Golf golf) {
			int i = 0;
			if(golf.brackets.length) {
				i = golf.brackets[$ - 1];
				golf.brackets = golf.brackets[0 .. $ - 1];
			}
			golf.stack = golf.stack[0 .. i] ~ [new GolfObject(ARRAY, golf.stack[i .. $])];
		});
		table["."] = Value((Golf golf) {
			auto a = golf.pop;
			golf.stack ~= [a, a];
		});
		table[";"] = Value((Golf golf) { if(golf.stack) golf.pop; });
		table["`"] = Value((Golf golf) {
			golf.stack ~= new GolfObject(STRING, golf.pop.inspect);
		});
		table["~"] = Value((Golf golf) {
			auto a = golf.pop;
			switch(a.type) {
			case NUMBER:
				golf.stack ~= new GolfObject(~a.number);
				break;
			case ARRAY:
				foreach(go; a.array) golf.stack ~= go;
				break;
			case STRING:
			case BLOCK:
				golf.exec(a.stringToString);
				break;
			}
		});
		table["\\"] = Value((Golf golf) {
			golf.stack ~= [golf.pop, golf.pop];
		});
		table["@"] = Value((Golf golf) {
			auto go = golf.pop;
			golf.stack ~= [golf.pop, go, golf.pop];
		});
		table["!"] = Value((Golf golf) {
			golf.stack ~= new GolfObject(golf.pop.not);
		});
		table["+"] = Value((Golf golf) {
			auto go = golf.pop;
			golf.stack ~= golf.pop + go;
		});
		table["-"] = Value((Golf golf) {
			auto go = golf.pop;
			golf.stack ~= golf.pop - go;
		});
		table["|"] = Value((Golf golf) {
			auto go = golf.pop;
			golf.stack ~= golf.pop | go;
		});
		table["&"] = Value((Golf golf) {
			auto go = golf.pop;
			golf.stack ~= golf.pop & go;
		});
		table["^"] = Value((Golf golf) {
			auto go = golf.pop;
			golf.stack ~= golf.pop ^ go;
		});
		table["("] = Value((Golf golf) {
			auto a = golf.pop;
			if(a.type == NUMBER) golf.stack ~= new GolfObject(a.number - 1);
			else {
				if(!a.array.length) throw new Exception("array is empty");
				golf.stack ~= new GolfObject(a.type, a.array[1 .. $]);
				golf.stack ~= a.array[0];
			}
		});
		table[")"] = Value((Golf golf) {
			auto a = golf.pop;
			if(a.type == NUMBER) golf.stack ~= new GolfObject(a.number + 1);
			else {
				if(!a.array.length) throw new Exception("array is empty");
				golf.stack ~= new GolfObject(a.type, a.array[0 .. $ - 1]);
				golf.stack ~= a.array[$ - 1];
			}
		});
		table["<"] = Value((Golf golf) {
			auto b = golf.pop;
			auto a = golf.pop;
			if(a.type < b.type) { auto c = a; a = b; b = c; }
			if(a.type == b.type) golf.stack ~= new GolfObject(a < b);
			else if(b.type == NUMBER) {
				int i = b.number;
				if(i < 0) i += a.array.length;
				int m = max(0, min!(int)(a.array.length, i));
				golf.stack ~= new GolfObject(a.type, a.array[0 .. m]);
			}
			else throw new Exception("invalid operation");
		});
		table[">"] = Value((Golf golf) {
			auto b = golf.pop;
			auto a = golf.pop;
			if(a.type < b.type) { auto c = a; a = b; b = c; }
			if(a.type == b.type) golf.stack ~= new GolfObject(a > b);
			else if(b.type == NUMBER) {
				int i = b.number;
				if(i < 0) i += a.array.length;
				int m = max(0, min!(int)(a.array.length, i));
				golf.stack ~= new GolfObject(a.type, a.array[m .. $]);
			}
			else throw new Exception("invalid operation");
		});
		table["="] = Value((Golf golf) {
			auto b = golf.pop;
			auto a = golf.pop;
			if(a.type < b.type) { auto c = a; a = b; b = c; }
			if(a.type == b.type) golf.stack ~= new GolfObject(a == b);
			else if(b.type == NUMBER) {
				int i = b.number;
				if(i < 0) i += a.array.length;
				if(i >= 0 && i < a.array.length) golf.stack ~= a.array[i];
				else throw new Exception("index out of range");
			}
			else throw new Exception("invalid operation");
		});
		table[","] = Value((Golf golf) {
			auto a = golf.pop;
			switch(a.type) {
			case NUMBER:
				auto q = new GolfObject[a.number];
				for(int i = 0; i < a.number; i++) q[i] = new GolfObject(i);
				golf.stack ~= new GolfObject(ARRAY, q);
				break;
			case ARRAY:
			case STRING:
				golf.stack ~= new GolfObject(a.array.length);
				break;
			case BLOCK:
				auto b = golf.pop;
				if(b.type == NUMBER) throw new Exception("invalid operation");
				string code = a.stringToString;
				GolfObject[] q;
				foreach(e; b.array) {
					golf.stack ~= e;
					golf.exec(code);
					if(!golf.pop.not) q ~= e;
				}
				golf.stack ~= new GolfObject(b.type, q);
				break;
			}
		});
		table["*"] = Value((Golf golf) {
			auto b = golf.pop;
			auto a = golf.pop;
			if(a.type <= b.type) { auto c = a; a = b; b = c; }
			switch(a.type) {
			case NUMBER:
				golf.stack ~= new GolfObject(a.number * b.number);
				break;
			case ARRAY:
			case STRING:
				if(b.type == NUMBER) {
					auto ar = new GolfObject[a.array.length * b.number];
					for(int i = 0; i < ar.length; i += a.array.length )
						ar[i .. i + a.array.length] = a.array[];
					golf.stack ~= new GolfObject(a.type, ar);
				}
				else {
					int t = (a.type == ARRAY) ? ARRAY : STRING;
					if(!b.array.length) {
						golf.stack ~= new GolfObject(t, b.array);
						break;
					}
					auto n = b.array[0].coerce(t);
					foreach(g; b.array[1 .. $]) n = n + a + g.coerce(t);
					golf.stack ~= n;
				}
				break;
			case BLOCK:
				string code = a.stringToString;
				if(b.type == NUMBER)
					for(int i = b.number; i; i--) golf.exec(code);
				else if(b.array.length) {
					golf.stack ~= b.array[0];
					foreach(g; b.array[1 .. $]) {
						golf.stack ~= g;
						golf.exec(code);
					}
				}
				break;
			}
		});
		table["%"] = Value((Golf golf) {
			auto b = golf.pop;
			auto a = golf.pop;
			if(a.type < b.type) { auto c = a; a = b; b = c; }
			switch(a.type) {
			case NUMBER:
				golf.stack ~= new GolfObject(a.number % b.number);
				break;
			case ARRAY:
			case STRING: // python's slice [::x]
				if(b.type == NUMBER) {
					if(b.number == 0) throw new Exception("slice step can't be zero");
					int i = (b.number > 0) ? 0 : a.array.length - 1;
					GolfObject[] ar = new GolfObject[a.array.length / abs(b.number)];
					foreach(ref g; ar) {
						g = a.array[i];
						i += b.number;
					}
					golf.stack ~= new GolfObject(a.type, ar);
				}
				else { // split
					GolfObject.coerce(a, b);
					GolfObject[] ar;
					int i = 0;
					while(i < a.array.length) {
						GolfObject[] q;
						while(i < a.array.length) {
							if(i + b.array.length <= a.array.length) {
								int j = 0;
								foreach(g; b.array) {
									if(g != a.array[i + j]) break;
									j++;
								}
								if(j == b.array.length) { // match ?
									i += j;
									break;
								}
							}
							q ~= a.array[i];
							i++;
						}
						if(q.length) ar ~= new GolfObject(a.type, q);
					}
					golf.stack ~= new GolfObject(ARRAY, ar);
				}
				break;

			case BLOCK:
				if(b.type == NUMBER) throw new Exception("invalid operation");
				string code = a.stringToString;
				GolfObject[] ar;
				foreach(i, g; b.array) {
					int lb = golf.stack.length;
					golf.stack ~= g;
					golf.exec(code);
					if(lb >= golf.stack.length) lb = golf.stack.length;
					ar ~= golf.stack[lb .. $];
					golf.stack = golf.stack[0 .. lb];
				}
				auto n = new GolfObject(ARRAY, ar);
				if(b.type == STRING) n = n.coerce(STRING);
				golf.stack ~= n;
				break;
			}
		});
		table["/"] = Value((Golf golf) {
			auto b = golf.pop;
			auto a = golf.pop;
			if(a.type < b.type) { auto c = a; a = b; b = c; }
			switch(a.type) {
			case NUMBER:
				if(b.number == 0) throw new Exception("division by zero");
				golf.stack ~= new GolfObject(a.number / b.number);
				break;
			case ARRAY:
			case STRING: // split into groups
				if(b.type == NUMBER) {
					if(b.number == 0) throw new Exception("group size can't be zero");
					int index = (b.number > 0) ? 0 : a.array.length - 1;
					int step = (b.number > 0) ? 1 : -1;
					int w = abs(b.number);
					int len = (a.array.length + w - 1) / w;
					int l = a.array.length;
					auto ar = new GolfObject[len];
					foreach(ref g; ar) {
						auto q = new GolfObject[min(w, l)];
						l -= w;
						foreach(ref e; q) {
							e = a.array[index];
							index += step;
						}
						g = new GolfObject(a.type, q);
					}
					golf.stack ~= new GolfObject(ARRAY, ar);
				}
				else { // split
					GolfObject.coerce(a, b);
					GolfObject[] ar;
					int i = 0;
					while(i <= a.array.length) {
						GolfObject[] q;
						while(i <= a.array.length) {
							if(i + b.array.length <= a.array.length) {
								int j = 0;
								foreach(g; b.array) {
									if(g != a.array[i + j]) break;
									j++;
								}
								if(j == b.array.length) { // match ?
									i += b.array.length;
									break;
								}
							}
							if(i < a.array.length) q ~= a.array[i];
							i++;
						}
						ar ~= new GolfObject(a.type, q);
					}
					golf.stack ~= new GolfObject(ARRAY, ar);
				}

				break;
			case BLOCK:
				string a_code = a.stringToString;
				switch(b.type) {
				case NUMBER: throw new Exception("invalid operation");
				case ARRAY:
				case STRING: // foreach
					foreach(g; b.array) {
						golf.stack ~= g;
						golf.exec(a_code);
					}
					break;
				case BLOCK: // unfold
					string b_code = b.stringToString;
					GolfObject[] ar;
					for(;;) {
						auto g = golf.pop;
						golf.stack ~= [g, g];
						golf.exec(a_code);
						if(golf.pop.not) break;
						ar ~= golf.stack[$ - 1];
						golf.exec(b_code);
					}
					golf.pop;
					golf.stack ~= new GolfObject(ARRAY, ar);
					break;
				}
				break;
			}
		});
		table["?"] = Value((Golf golf) {
			auto b = golf.pop;
			auto a = golf.pop;
			if(a.type < b.type) { auto c = a; a = b; b = c; }
			switch(a.type) {
			case NUMBER:
				golf.stack ~= new GolfObject(cast(Number) pow(a.number, cast(real) b.number));
				break;
			case ARRAY:
			case STRING:
				GolfObject f = null;
				foreach(i, g; a.array)
					if(g == b) {
						f = new GolfObject(i);
						break;
					}
				golf.stack ~= (f) ? f : new GolfObject(-1);
				break;
			case BLOCK:
				if(b.type == NUMBER || b.type == BLOCK)
					throw new Exception("invalid operation");
				string code = a.stringToString;
				foreach(g; b.array) {
					golf.stack ~= g;
					golf.exec(code);
					if(!golf.pop.not) {
						golf.stack ~= g;
						break;
					}
				}
				break;
			}
		});
		table["$"] = Value((Golf golf) {
			auto a = golf.pop;
			switch(a.type) {
			case NUMBER: // push element on stack
				int i = golf.stack.length - 1 - a.number;
				if(a.number < 0) i = -1 - a.number;
				if(i < 0 && i >= golf.stack.length)
					throw new Exception("invalid stack index");
				golf.stack ~= golf.stack[i];
				break;
			case ARRAY:
			case STRING: // sort
				golf.stack ~= new GolfObject(a.type, a.array.dup.sort);
				break;
			case BLOCK:
				string code = a.stringToString;
				auto b = golf.pop;
				if(b.type == NUMBER) throw new Exception("invalid operation");
				auto ar = b.array.dup;
				sort(ar, (GolfObject g) {
					golf.stack ~= g;
					golf.exec(code);
					return golf.pop;
				});
				golf.stack ~= new GolfObject(b.type, ar);
				break;
			}
		});
		table["do"] = Value((Golf golf) {
			auto a = golf.pop;
			if(a.type != BLOCK) throw new Exception("invalid operation");
			string code = a.stringToString;
			for(;;) {
				golf.exec(code);
				if(golf.pop.not) break;
			}
		});
		table["zip"] = Value((Golf golf) {
			auto a = golf.pop;
			if(a.type != ARRAY) throw new Exception("invalid operation");
			if(a.array.length == 0) {
				golf.stack ~= a;
				return;
			}
			foreach(elem; a.array)
				if(elem.type == NUMBER) throw new Exception("invalid operation");
			int len = a.array[0].array.length;
			int type = a.type;
			foreach(elem; a.array[1 .. $]) {
				len = min!(int)(len, elem.array.length);
				type = min(type, elem.type);
			}
			auto ar = new GolfObject[len];
			foreach(i, ref elem; ar) {
				auto q = new GolfObject[a.array.length];
				foreach(j, ref go; q) go = a.array[j].array[i];
				elem = new GolfObject(type, q);
			}
			golf.stack ~= new GolfObject(ARRAY, ar);
		});
		table["print"] = Value((Golf golf) {
			writef("%s", golf.pop.toString);
		});
		table["puts"] = Value(null, new GolfObject(BLOCK, "print n print"));
		table["p"] = Value(null, new GolfObject(BLOCK, "`puts"));
	}
}

