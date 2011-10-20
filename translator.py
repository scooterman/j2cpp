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
	
	expression = type('Expression', (), {})
	expression.value = ''
	
	Java.translate(node.conditionalExpression, expression)
	
	if node.assignmentOperator:
		expression.value += Java.translate(node.assignmentOperator, expression)

	if node.expression:
		expression.value += Java.translate(node.expression, expression)
	
	scope.expression = expression

'''
def conditionalExpression(rule):
	rule | (conditionalOrExpression, [ '?', expression, ':', expression ])

	rule.astAttrs = { 'condor' : conditionalOrExpression , 'cond' : [expression] }
'''
@Java.translates(ast.Conditionalexpression)
def translate_Conditionalexpression(node, scope):
	value = ''
	value += Java.translate(node.condor, scope)
	
	if node.cond:
		value += ' ? {0} : {1}'.format(Java.translate(node.cond[0]),Java.translate(node.cond[1]))
	
	return value

@Java.translates(ast.Conditionalorexpression)
def translate_Conditionalorexpression(node, scope):
	value = ''
	
	if node.condand[0]:
		value += Java.translate(node.condand[0], scope)
		
		if len(node.condand) > 1:
			value += ' || '
			for cond in node.condand[1:]:
				value += Java.translate(cond, scope)

	return value
	
@Java.translates(ast.Conditionalandexpression)
def translate_conditionalAndExpression(node, scope):
	value = ''

	if node.inclusiveor:
		value += Java.translate(node.inclusiveor[0], scope)
		
		if len(node.inclusiveor) > 1:
			value += ' && '
			for cond in node.inclusiveor[1:]:
				value += Java.translate(cond, scope)
	return value

@Java.translates(ast.Inclusiveorexpression)
def translate_Inclusiveorexpression(node, scope):
	value = ''
	if node.exclusiveor:
		value = Java.translate(node.exclusiveor[0], scope)
		
		print 'exclusive or value: "{0}"'.format(value)
		
		if len(node.exclusiveor) > 1:
			value += ' | '
			for cond in node.exclusiveor[1:]:
				value += Java.translate(cond)
	
	return value
	

@Java.translates(ast.Exclusiveorexpression)
def translate_Exclusiveorexpression(node, scope):
	value = ''
	if node.andexpr:
		value = Java.translate(node.andexpr[0], scope)
		
		if len(node.andexpr) > 1:
			value += ' ^ '
			for cond in node.andexpr[1:]:
				value += Java.translate(cond)
	
	return value


@Java.translates(ast.Andexpression)
def translate_Andexpression(node, scope):
	value = ''
	if node.equalexpr:
		value = Java.translate(node.equalexpr[0], scope)
		
		if len(node.equalexpr) > 1:
			value += ' & '
			for cond in node.equalexpr[1:]:
				value += Java.translate(cond)
	
	return value
	

@Java.translates(ast.Equalityexpression)
def translate_Equalityexpression(node, scope):
	value = ''
	if node.instanceof:
		value = Java.translate(node.instanceof[0], scope)
		
		if len(node.instanceof) > 1:
			value += ' {0} '.format(map(lambda x: str(x) , node.comp))
			
			for cond in node.instanceof[1:]:
				value += Java.translate(cond)
	
	return value

@Java.translates(ast.Instanceofexpression)
def translate_Instanceofexpression(node, scope):
	value = ''
	if node.relational:
		value = Java.translate(node.relational, scope)
		
		if node.type:
			value += ' instanceof ' + Java.translate(node.type)

	return value

@Java.translates(ast.Relationalexpression)
def translate_Relationalexpression(node, scope):
	value = ''
	if node.shift:
		value = Java.translate(node.shift[0], scope)
		
		if node.relationalOp:
			for op, se in zip(node.relationalOp, node.shift[1:]):
				value += Java.translate(op, scope) + ' ' + java.translate(se, scope)

	return value

@Java.translates(ast.Shiftexpression)
def translate_Shiftexpression(node, scope):
	value = ''
	if node.additive:
		value = Java.translate(node.additive[0], scope)
		
		if node.shift:
			for op, se in zip(node.shift, node.additive[1:]):
				value += Java.translate(op, scope) + ' ' + java.translate(se, scope)

	return value

@Java.translates(ast.Additiveexpression)
def translate_Additiveexpression(node, scope):
	value = ''
	print dir(node)
	if node.mulexpr:
		value = Java.translate(node.mulexpr[0], scope)

		if node.op:
			for op, se in zip(node.op, node.mulexpr[1:]):
				value += Java.translate(op, scope) + ' ' + java.translate(se, scope)

	return value

@Java.translates(ast.Multiplicativeexpression)
def translate_Multiplicativeexpression(node, scope):
	value = ''
	if node.unary:
		value = Java.translate(node.unary[0], scope)

		if node.op:
			for op, se in zip(node.op, node.unary[1:]):
				value += Java.translate(op, scope) + ' ' + java.translate(se, scope)

	return value
	
	
@Java.translates(ast.Unaryexpression)
def translate_Unaryexpression(node, scope):
	value = ''
	if node.symbol:
		value = ''.join(node.symbol) + Java.translate(node.unary)
	elif node.notplusminus:
		value = Java.translate(node.notplusminus)
	
	return value
	
@Java.translates(ast.Unaryexpressionnotplusminus)
def translate_unaryExpressionNotPlusMinus(node, scope):
	value = ''
	if node.symbol:
		value = ''.join(node.symbol) + Java.translate(node.unary, scope)
	elif node.cast:
		value = Java.translate(node.cast, scope)
	elif node.primary:
		value = Java.translate(node.primary, scope)
		
	return value

@Java.translates(ast.Primaryexpression)
def translate_Primaryexpression(node, scope):
	value = ''
	if node.primary:
		value = Java.translate(node.primary, scope)
	
	if node.selector:
		for s in node.selector:
			value += Java.translate(s, scope)
	
	if node.op:
		for op in node.op:
			value = Java.translate(op, scope)
		
	return value

@Java.translates(ast.Primary)
def translate_primary(node, scope):
	if node.parExpression:
		return Java.translate(node.parExpression, scope)
	elif node.superSuffix:
		return 'super ' + Java.translate(node.superSuffix, scope)
	elif node.literal:
		return str(node.literal)
	elif node.creator:
		return 'new ' + Java.translate(node.creator)
	elif node.id:
		value = Java.translate(node.id[0], scope)
		if len(node.id) > 1:
			for i in node.id:
				value += '::' + Java.translate(i, scope)
		if node.suffix:
			value += Java.translate(node.suffix, scope)

	elif node.primitive:
		return Java.translate(node.primitive)

		
@Java.translates(ast.Blockstatement)
def translate_Blockstatement(node, scope):
	