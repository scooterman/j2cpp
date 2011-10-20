from codetalker.pgm import Translator
import grammar
import itertools

Java = Translator(grammar.java_grammar, imports = [] , classes = [])
ast = grammar.java_grammar.ast_classes

@Java.translates(ast.Compilationunit)
def translate_compilationUnit(node, scope):
	for i in node.imports:
		scope.imports.append(Java.translate(i, scope))

	for td in node.typeDeclarations:
		decl = Java.translate(td, scope)
		scope.classes.append(decl)
		
	return scope

@Java.translates(ast.Typedeclaration)
def translate_typeDeclaration(node, scope):
	if node.classDec:
		return Java.translate(node.classDec, scope)

@Java.translates(ast.Classorinterfacedeclaration)
def translate_class_or_interface_decl(node, scope):
	modifiers = [ Java.translate(n, scope) for n in node.modifiers ]

	df = None
	if node.classDec:
		df = type('ClassDefinition', (), {})
		Java.translate(node.classDec[0], df)

	elif node.interfaceDec:
		df = type('InterfaceDefinition', (), {})
		Java.translate(node.interfaceDec[0], df)

	df.modifiers = modifiers
	return df	

@Java.translates(ast.ImportDeclaration)
def translate_importDeclaration(node, scope):
	return '/'.join(map(lambda x: Java.translate(x), node.importName))

@Java.translates(ast.Normalclassdeclaration)
def translate_normalClassDeclaration(node, scope):
	scope.name = Java.translate(node.className)
	print 'normalclassdeclaration', scope.name
	scope.body = Java.translate(node.body, scope)

''' contains a class body declaration

class A {
--- > BODY BEGINS

Body declaration

---- < BODY ENDS
}

'''
@Java.translates(ast.Classbody)
def translate_Classbody(node, scope):
	print 'classbody'
	scope.class_def = scope
	scope.variables = []
	scope.methods = []
	scope.classes = []
	scope.static_constructor = None

	if node.decl:
		for decl in node.decl:
			Java.translate(decl, scope)

@Java.translates(ast.Classbodydeclaration)
def translate_Classbodydeclaration(node, scope):
	if node.static:
		static_constructor = type('ClassStaticConstructor', (), {})

		static_constructor.body = Java.translate(node.body, scope)
		static_constructor.body.modifiers.append('static')
		scope.static_constructor = static_constructor

	elif node.modifiers:
		modifiers = []
		for m in modifiers:
			modifiers.append(Java.translate(m))
		member = Java.translate(node.memberDecl, scope)
		member.modifiers = modifiers

@Java.translates(ast.Classorinterfacemodifier)
def translate_Classorinterfacemodifier(node, scope):
	if node.modifier:
		return str(node.modifier)

@Java.translates(ast.Memberdecl)
def translate_memberDecl(node, scope):
	if node.voidMethod:
		print 'voidMethod'
		return Java.translate(node.voidMethod, scope)
	elif node.interfaceDeclaration:
		pass
	elif node.classDeclaration:
		return Java.translate(node.classDeclaration, scope)
	elif node.memberDeclaration:
		return Java.translate(node.memberDeclaration, scope)
	elif node.constructorDecl:
		pass

@Java.translates(ast.Memberdeclaration)
def translate_Memberdeclaration(node, scope):
	print 'memberdeclaration: is method?', node.methodDeclaration, 'isfield? ', node.fieldDeclaration
	if node.methodDeclaration:
		method = type('MethodDeclaration', (), {})
		method.return_type = Java.translate(node.type, scope)
		Java.translate(node.methodDeclaration, method)
		scope.methods.append(method)
		print 'member declaration scope:', scope.methods
		return method
	elif node.fieldDeclaration:
		variables = type('VariablesDeclaration', (), {})
		variables.declarations = []
		Java.translate(node.fieldDeclaration, variables)
		scope.variables.append(variables)
		return variables
		

@Java.translates(ast.Fielddeclaration)
def translate_Fielddeclaration(node, scope):
	print 'fielddeclaration:', node.variables
	for variable in node.variables:
		print variable
		v = type('Variable', (), {})
		Java.translate(variable, v)
		scope.declarations.append(v)

@Java.translates(ast.Type)
def translate_Type(node, scope):
	print 'parsing type', 'scope is:',  scope
	_type = type('Type', (), {})
	_type.args = []

	if node.primitiveType:
		print 'is primitive'
	elif node.classOrInterfaceType:
		Java.translate(node.classOrInterfaceType, _type)

	return _type

@Java.translates(ast.Classorinterfacetype)
def translate_Classorinterfacetype(node, scope):	
	print node.types
	for t,arg in itertools.izip_longest(node.types, node.arguments):
		tp = str(t)
		if arg:
			tp += Java.translate(arg)
		scope.args.append(tp)		

@Java.translates(ast.Voidmethoddecl)
def translate_Voidmethoddecl(node, scope):
	method = type('MethodDeclaration', (), {})
	method.name = Java.translate(node.name)
	method.return_type = 'void'
	method.parameters = []
	Java.translate(node.rest, method)
	scope.methods.append(method)

	print 'void method: name %s, return type %s, parameters %s, body %s' %(method.name, method.return_type, method.parameters, method.block)

	return method

@Java.translates(ast.Voidmethoddeclaratorrest)
def translate_Voidmethoddeclaratorrest(node, method):
	Java.translate(node.parameters, method)
	if node.methodBody:
		Java.translate(node.methodBody.block, method)

@Java.translates(ast.Formalparameters)
def translate_Formalparameters(node, scope):
	for param in node.parameters:
		scope.parameters.append(Java.translate(param))
	
	print 'formal parameters:', scope.parameters

@Java.translates(ast.Formalparametervariable)
def translate_Formalparametervariable(node, scope):
	var = type('Variable', (), {})
	var.modifiers = []

	for m in node.modifiers:
		var.modifiers.append(Java.translate(node.modifiers, scope))

	var.type = Java.translate(node.type, var)
	Java.translate(node.name, var)

	print 'variable name:' , var.name, 'var type:', var.type.args
	return var

@Java.translates(ast.Variabledeclaratorid)
def translate_Variabledeclaratorid(node, scope):
	print 'variable declarator: cocada %s has array? %s' %( Java.translate(node.name), node.array)
	scope.name = Java.translate(node.name)

@Java.translates(ast.Block)
def translate_Block(node, scope):
	block = type('Block',(), {})
	block.statements = []

	for statement in node.blockStatement:
		Java.translate(statement, block)

	print 'block statements', block.statements

	scope.block = block

@Java.translates(ast.Identifier)
def translate_Identifier(node, scope):
	print 'identifier', node.id
	return str(node.id)

@Java.translates(ast.Variableinitializer)
def translate_Variableinitializer(node, scope):
	print 'variable initializer'
	if node.arrayInitializer:
		Java.translate(node.arrayInitializer, scope)
	elif node.expression:
		Java.translate(node.expression, scope)

@Java.translates(ast.Expression)
def translate_Expression(node, scope):
	print 'expression'
	expr = node.conditionalExpression 
	
	while type(expr[0]) == list:
		expr = expr.pop()

	expression = type('Expression', (), {})
	expression.first = expr
	if node.assignmentOperator:
		node.operator = Java.translate(node.assignmentOperator, expression)

	if node.expression:
		Java.translate(node.expression, expression)

