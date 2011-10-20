from .. import grammar

'''def test_expression_0():
	expr = 'x = 10'
	
	ast = grammar.java_grammar.get_ast(expr, grammar.expression)'''

def test_expression_1():
	expr = '''value == 10'''

	ast = grammar.java_grammar.get_ast(expr, grammar.expression)