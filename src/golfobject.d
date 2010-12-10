module src.golfobject;

import std.stdio;
import std.string;

import src.golf;

T max(T)(T t1, T t2) { return t1 > t2 ? t1 : t2; }
T min(T)(T t1, T t2) { return t1 < t2 ? t1 : t2; }

T[] map(S, T)(S[] source, T delegate(S) func) {
	T[] target = new T[source.length];
	foreach(i, s; source) target[i] = func(s);
	return target;
}

void sort(T, S)(T[] list, S delegate(T) mapping) {
	if(list.length <= 1) return;
	uint i = 0;
	while(i < list.length) {
		if(i == 0 || mapping(list[i]) >= mapping(list[i - 1])) i++;
		else {
			T t = list[i];
			list[i] = list[i - 1];
			list[i - 1] = t;
			i--;
		}
	}
}

alias int Number;
enum { NUMBER, ARRAY, STRING, BLOCK }

class GolfObject {
	int					type;
	union {
		Number			number;
		GolfObject[]	array;
	}

	this(Number n) {
		type = NUMBER;
		number = n;
	}
	this(int t, GolfObject[] a) {
		type = t;
		array = a;
	}
	this(int t, string s) {
		type = t;
		array = map(s, (char c) { return new GolfObject(c); });
	}
	string stringToString() {
		return map(array, (GolfObject go) { return cast(char) go.number; });
	}
	string toString() {
		switch(type) {
		case NUMBER:
			return std.string.toString(number);
		case ARRAY: 
			return join(map(array, (GolfObject go) { return go.toString; }), "");
		case STRING:
			return stringToString();
		case BLOCK:
			return "{" ~ map(array, (GolfObject go) {
				return cast(char) go.number;
			}) ~ '}';
		}
	}
	string inspect() {
		switch(type) {
		case NUMBER: return toString;
		case ARRAY:
			return "[" ~ join(map(array, (GolfObject go) {
				return go.inspect;
			}), " ") ~ "]";
		case STRING:
			string s = "\"";
			foreach(go; array) {
				char c = cast(char) go.number;
				if(c == '"' || c == '\\') s ~= '\\';
				s ~= c;
			}
			return s ~ '"';
		case BLOCK: return toString;
		}
	}

	bool not() {
		if(type == NUMBER) return number == 0;
		return array.length == 0;
	}

	GolfObject coerce(int t) {
		GolfObject[] flatten(GolfObject a[]) {
			GolfObject[] res;
			foreach(e; a) {
				if(e.type == NUMBER) res ~= e;
				else if(e.type == ARRAY) res ~= flatten(e.array);
				else res ~= e.array;
			}
			return res;
		}
		if(t <= type) return this;
		switch(type) {
		case NUMBER:
			switch(t) {
			case ARRAY: return new GolfObject(ARRAY, [this]);
			case STRING:
			case BLOCK: return new GolfObject(t, toString);
			}
		case ARRAY:
			switch(t) {
			case STRING: return new GolfObject(STRING, flatten(array));
			case BLOCK:
				string s;
				if(array.length) {
					s ~= array[0].toString;
					foreach(g; array[1 .. $]) s ~= ' ' ~ g.toString;
				}
				return new GolfObject(BLOCK, s);
			}
		case STRING: return new GolfObject(BLOCK, array);
		}
	}
	static void coerce(ref GolfObject a, ref GolfObject b) {
		a = a.coerce(b.type);
		b = b.coerce(a.type);
	}

	int opEquals(Object o) {
		GolfObject go = cast(GolfObject) o;
		if(type == NUMBER && go.type == NUMBER) return number == go.number;
		return array == go.array;
	}
	int opCmp(Object o) {
		GolfObject go = cast(GolfObject) o;
		switch(type) {
		case NUMBER:
			if(go.type == NUMBER) return number - go.number;
			return type - go.type;
		case ARRAY:
		case STRING:
		case BLOCK:
			int r = array.length - go.array.length;
			for(int i = 0; i < min(array.length, go.array.length); i++) {
				int q = array[i].opCmp(go.array[i]);
				if(q != 0) return q;
			}
			return r;
		}
	}
	hash_t toHash() {
		if(type == NUMBER) return number;
		hash_t h = 0;
		foreach(g; array) h = (h * 37) + g.toHash();
		return h;
	}

	GolfObject opAdd(GolfObject o) {
		coerce(this, o);
		switch(type) {
			case NUMBER: return new GolfObject(number + o.number);
			case ARRAY:
			case STRING: return new GolfObject(type, array ~ o.array);
			case BLOCK: return new GolfObject(BLOCK, array ~ new GolfObject(' ') ~ o.array);
		}
	}
	GolfObject opSub(GolfObject o) {
		coerce(this, o);
		switch(type) {
		case NUMBER: return new GolfObject(number - o.number);
		case ARRAY:
		case STRING:
		case BLOCK:
			bool[GolfObject] set;
			GolfObject[] ar;
			foreach(e; o.array) set[e] = true;
			foreach(g; array) if(!(g in set)) ar ~= g;
			return new GolfObject(type, ar);
		}
	}
	GolfObject opAnd(GolfObject o) {
		coerce(this, o);
		switch(type) {
		case NUMBER: return new GolfObject(number & o.number);
		case ARRAY:
		case STRING:
		case BLOCK:
			bool[GolfObject] set;
			GolfObject[] ar;
			foreach(e; o.array) set[e] = true;
			foreach(g; array)
				if(g in set) {
					ar ~= g;
					set.remove(g);
				}
			return new GolfObject(type, ar);
		}
	}
	GolfObject opOr(GolfObject o) {
		coerce(this, o);
		switch(type) {
		case NUMBER: return new GolfObject(number | o.number);
		case ARRAY:
		case STRING:
		case BLOCK:
			bool[GolfObject] set;
			GolfObject[] ar;
			foreach(g; array ~ o.array)
				if(!(g in set)) {
					ar ~= g;
					set[g] = true;
				}
			return new GolfObject(type, ar);
		}
	}
	GolfObject opXor(GolfObject o) {
		coerce(this, o);
		switch(type) {
		case NUMBER: return new GolfObject(number ^ o.number);
		case ARRAY:
		case STRING:
		case BLOCK:
			bool[GolfObject] set;
			GolfObject[] ar;
			foreach(e; array) set[e] = true;
			foreach(e; o.array) {
				if(e in set) set.remove(e);
				else set[e] = true;
			}
			foreach(g; array ~ o.array)
				if(g in set) {
					ar ~= g;
					set.remove(g);
				}
			return new GolfObject(type, ar);
		}
	}

public:
	Number getNumber() {
		if(type != NUMBER) throw new Exception("object not a number");
		return number;
	}
	string getString() {
		if(type != STRING) throw new Exception("object not a string");
		return stringToString;
	}

}


