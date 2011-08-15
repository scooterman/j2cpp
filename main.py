import os,sys
import re

sys.setrecursionlimit(20000)

import_table = {}

class index_holder:
	idx = 0

def lex_parse(data, handler, index):
	#print 'data is "', data[index.idx:index.idx + 50], '"'
	for hnd, action in handler:
		res = hnd.match(data[index.idx:])
 
		if res:
			print 'matched: ', res.group(0)
			index.idx += res.span()[1]
			return action(res, data, handler, index)

	return []

def close_scope(index, end, result):
	index.idx += end
	return result

nop = lambda res, data, hnd, index: lex_parse(data, hnd, index)

lex_handlers_comment = [ 
	(re.compile(r'[^\*\\]+') , lambda res, data, hnd, index: ['literal', res.group(0)]),
]

lex_handlers_single_comment = [
	(re.compile('[^\n]+'), lambda res, data, hnd, index: [ 'literal', res.group(0) ]),
]

lex_handlers = [  
	(re.compile('/\*') , sys.setrecursionlimit
		lambda res, data, hnd, index: 
		[ 
                  [
			'/*',
			lex_parse(data, lex_handlers_comment, index),
			close_scope(index, 2, "*/") 
		  ]
		] + lex_parse(data, hnd, index)
	),
	(re.compile('//'),
		lambda res, data, hnd, index:
		[
			[
				'//',
				lex_parse(data, lex_handlers_single_comment, index),
				close_scope(index, 2, None)
			]
		] + lex_parse(data, hnd, index)
	),
	(re.compile('\('),
		lambda res, data, hnd, index: 
		[
		  ['('] + lex_parse(data, hnd, index)
		] + lex_parse(data, hnd, index)
	),
	(re.compile('{'),
		lambda res, data, hnd, index:
		[
		  ['{'] + lex_parse(data, hnd, index)
		] + lex_parse(data,hnd, index)
	),
	(re.compile('[a-z|A-Z]+[0-9|a-z|A-Z|_]*'),
		lambda res, data, hnd, index:
			[('literal', res.group(0))] + lex_parse(data, hnd, index)
	),
	(re.compile(r'[0-9]+\.[0-9]*|[0-9|_]+'),
		lambda res, data, hnd, index:
			[('number', res.group(0))] + lex_parse(data, hnd, index)
	),
	(re.compile(';'), 
		lambda res, data, hnd, index:
		[
		  ('comma')
		] + lex_parse(data, hnd, index)	
	),

	(re.compile(r'\+|-|=|\*|/|>|<|!|&|%|\|:|\.'), 
		lambda res, data, hnd, index:
		[ ( 'operator',
		    res.group(0)) ] +
		  lex_parse(data, hnd, index)
			
	),

	(re.compile('(\s|\n)+') , nop),
	(re.compile(','), nop),
	(re.compile('\)|\}'), lambda res, data, hnd, index: close_scope(index, 0, [res.group(0)])),
]

def parse( filename, hnd, idx ):
	f = open(filename, "r")
	data = f.read()	
	
	lex_parse(data, hnd, idx)

test = '''

/*
teste
*/

int1ovelha2cocada2 1.123 main (ovelha, lhama, macaco) {

	int x = new Data();

}
'''

if __name__ == "__main__":
	index = index_holder()
	lex = parse(sys.argv[1], lex_handlers, index)
	print lex	
	
