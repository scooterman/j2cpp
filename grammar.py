from codetalker.pgm import Grammar, Translator
from codetalker.pgm.special import star, plus, _or, commas
from codetalker.pgm.tokens import *
from codetalker.pgm.tokenize import tokenize
from codetalker.pgm.text import Text

def compilationUnit(rule):
	rule | (annotations, 
			_or(
				(
					packageDeclaration,
					star(importDeclaration),
					star(typeDeclaration)
				) ,
				(
					classOrInterfaceDeclaration, star(typeDeclaration)
				)
			)
		) | (
			[packageDeclaration], 
			star(importDeclaration), 
			star(typeDeclaration)
		    )	

	rule.astAttrs = { 'packageDeclaration' : packageDeclaration , 'imports' : [importDeclaration] , 'typeDeclarations' : [classOrInterfaceDeclaration] + [typeDeclaration] }

def packageDeclaration(rule):
	rule | ( 'package', qualifiedName, ';')

def importDeclaration(rule):
	rule | ('import', ['static'], qualifiedName, ['.' , '*'], ';')
	rule.astAttrs = { 'importName' : qualifiedName }

importDeclaration.astName = 'ImportDeclaration'

def typeDeclaration(rule):
	rule | classOrInterfaceDeclaration | ';'
	rule.astAttrs = { 'classDec' : classOrInterfaceDeclaration }

def classOrInterfaceDeclaration(rule):
	rule | ( star(classOrInterfaceModifier) , _or(classDeclaration,interfaceDeclaration))
	rule.astAttrs = { 'modifiers' : [classOrInterfaceModifier], 'classDec' : classDeclaration, 'interfaceDec' : interfaceDeclaration }

def classOrInterfaceModifier(rule):
	rule | annotation | 'public' | 'protected' | 'private' | 'abstract' | 'static' | 'final' | 'strictp'
	rule.astAttrs = { 'annotation' : annotation, 'modifier' : ID }	

def modifiers(rule):
	rule | star(modifier)

def classDeclaration(rule):
	rule | normalClassDeclaration | enumDeclaration

def normalClassDeclaration(rule):
	rule | ('class', Identifier, [typeParameters], ['extends', _type], ['implements' , commas(_type)], classBody)
	rule.astAttrs = { 'className' : Identifier, 'extends' : _type, 'body' : classBody }

def typeParameters(rule):
	rule | ('<', typeParameter, star(',', typeParameter), '>')


def typeParameter(rule):
	rule | (Identifier, ['extends', typeBound])

def typeBound(rule):
	rule | ( _type, star('&', _type))

def enumDeclaration(rule):
	rule | (ENUM, Identifier, ['implements', commas(_type)], enumBody)

def enumBody(rule):
	rule | ('{', [enumConstants], [','], [enumBodyDeclarations], '}')

def enumConstants(rule):
	rule | (enumConstant, star(',', enumConstant))

def enumConstant(rule):
	rule | ([annotations], Identifier, [arguments], [classBody])

def enumBodyDeclarations(rule):
	rule | (';', star(classBodyDeclaration))

def interfaceDeclaration(rule):
	rule | normalInterfaceDeclaration | annotationTypeDeclaration

def normalInterfaceDeclaration(rule):
	rule | ('interface', Identifier, [typeParameters], ['extends', commas(_type)], interfaceBody)

def classBody(rule):
	rule | ('{', star(classBodyDeclaration), '}')
	rule.astAttrs = { 'decl' : [classBodyDeclaration] }

def interfaceBody(rule):
	rule | ('{', star(interfaceBodyDeclaration), '}')

def classBodyDeclaration(rule):
	rule | ';' | (['static'], block) | (modifiers, memberDecl)
	rule.astAttrs = { 'static' : ID , 'block' : block , 'modifiers' : modifiers, 'memberDecl' : memberDecl }	

def memberDecl(rule):
	rule | genericMethodOrConstructorDecl \
	|   voidMethodDecl \
	|   memberDeclaration \
    |   constructorDecl \
    |   interfaceDeclaration \
    |   classDeclaration
	
	rule.astAttrs = { 'voidMethod' : voidMethodDecl , 
			  'interfaceDeclaration' : interfaceDeclaration,
			  'constructorDecl' : constructorDecl,
			  'interfaceDeclaration' : interfaceDeclaration,
			  'classDeclaration' : classDeclaration,
			  'memberDeclaration' : memberDeclaration }

def constructorDecl(rule):
	rule | (Identifier, constructorDeclaratorRest)
	rule.astAttrs = { "name" : Identifier, 'rest' : constructorDeclaratorRest }

def voidMethodDecl(rule):
	rule | ('void', Identifier, voidMethodDeclaratorRest)
	rule.astAttrs = { 'name' : Identifier, 'rest' : voidMethodDeclaratorRest }

def memberDeclaration(rule):
	rule | (_type , _or(methodDeclaration , fieldDeclaration))
	rule.astAttrs = { "type" : _type, "methodDeclaration" : methodDeclaration, "fieldDeclaration" : fieldDeclaration }

def genericMethodOrConstructorDecl(rule):
	rule | (typeParameters, genericMethodOrConstructorRest) 

def genericMethodOrConstructorRest(rule):
	rule | (_or(_type , 'void') , Identifier, methodDeclaratorRest) | (Identifier, constructorDeclaratorRest)

def methodDeclaration(rule):
	rule | (Identifier, methodDeclaratorRest)
	rule.astAttrs = { 'methodName' : Identifier, 'rest' : methodDeclaratorRest }

def fieldDeclaration(rule):
	rule | (commas(variableDeclarator), ';')
	rule.astAttrs = { 'variables' : variableDeclarator }

def variableDeclarator(rule):
	rule | (variableDeclaratorId, ['=', variableInitializer])
	rule.astAttrs = { 'name' : variableDeclaratorId, 'initializer' : variableInitializer }

def interfaceBodyDeclaration(rule):
	rule | (modifiers, interfaceMemberDecl) | ';'

def interfaceMemberDecl(rule):
	rule | interfaceMethodOrFieldDecl | interfaceGenericMethodDecl | ('void', Identifier, voidInterfaceMethodDeclaratorRest) | interfaceDeclaration | classDeclaration

def interfaceMethodOrFieldDecl(rule):
	rule | (_type, Identifier, interfaceMethodOrFieldRest)

def interfaceMethodOrFieldRest(rule):
	rule | (constantDeclaratorsRest, ';') | interfaceMethodDeclaratorRest

def methodDeclaratorRest(rule):
	rule | (formalParameters, star('[' ']'), ['throws', commas(qualifiedName)], _or(methodBody, ';'))

def voidMethodDeclaratorRest(rule):
	rule | (formalParameters, ['throws', commas(qualifiedName)], _or(methodBody, ';'))
	rule.astAttrs = { 'parameters' : formalParameters, 'methodBody' : methodBody } 

def interfaceGenericMethodDecl(rule):
	rule | ( typeParameters, _or(_type , 'void'), Identifier,
        interfaceMethodDeclaratorRest)

def interfaceMethodDeclaratorRest(rule):
	rule | (formalParameters, star('[' ']'), ['throws', commas(qualifiedName)], ';')

def voidInterfaceMethodDeclaratorRest(rule):
	rule | (formalParameters, ['throws', commas(qualifiedName)], ';')

def constructorDeclaratorRest(rule):
	rule | (formalParameters, ['throws', commas(qualifiedName)], constructorBody)

def constantDeclarator(rule):
	rule | (Identifier, constantDeclaratorRest)

def variableDeclarators(rule):
	rule | (variableDeclarator, star(',', variableDeclarator))

def variableDeclarator(rule):
	rule | (variableDeclaratorId, ['=', variableInitializer])

def constantDeclaratorsRest(rule):
	rule | (constantDeclaratorRest, star(',', constantDeclarator))

def constantDeclaratorRest(rule):
	rule | (star('[',']'), '=', variableInitializer)

def variableDeclaratorId(rule):
	rule | (Identifier, star('[', ']'))
	rule.astAttrs = { 'name' : Identifier, 'array' : SYMBOL }

def variableInitializer(rule):
	rule |  arrayInitializer | expression
	rule.astAttrs = { 'arrayInitializer' : arrayInitializer, 'expression' : expression }

def arrayInitializer(rule):
	rule | ('{', [ commas(variableInitializer) ], '}')
	rule.astAttrs = { 'variableInitializer' : variableInitializer }

def modifier(rule):
	rule | 'public' |'protected'\
	|   'private' \
	|   'static' \
	|   'abstract' \
	|   'final'  \
	|   'native' \
	|   'synchronized'\
	|   'transient' \
	|   'volatile'\
	|   'strictfp'\

def packageOrTypeName(rule):
	rule | qualifiedName

def enumConstantName(rule):
	rule | Identifier

def typeName(rule):
	rule | qualifiedName

def _type(rule):
	rule | (classOrInterfaceType, star('[',']')) \
	     | (primitiveType, star('[', ']')) 
	
	rule.astAttrs = { 'classOrInterfaceType' : classOrInterfaceType, 'primitiveType' : primitiveType }

def classOrInterfaceType(rule):
	rule | (Identifier, [typeArguments], star('.' ,Identifier, [typeArguments] ))
	rule.astAttrs = { 'types' : [Identifier], 'arguments' : [typeArguments] }

def primitiveType(rule):
	rule | 'boolean' \
	    |   'char'  \
	    |   'byte' \
	    |   'short' \
	    |   'int' \
	    |   'long' \
	    |   'float' \
	    |   'double'
	rule.astAttrs = { 'id' : ID }
	    
def variableModifier(rule):
	rule | 'final' | annotation
	rule.astAttrs = { 'final' : ID, 'annotation' : annotation }

def typeArguments(rule):
	rule | ('<', typeArgument, star(commas(typeArgument)), '>')
	rule.astAttrs = { 'args' : [typeArgument] }

def typeArgument(rule):
	rule | _type | typeArgumentRule
	rule.astAttrs = { 'type' : _type }

def typeArgumentRule(rule):
	rule | ('?', [_or('extends' , 'super'), _type])

def formalParameters(rule):
	rule | ('(', [commas(formalParameterDecls)], ')')
	rule.astAttrs = { 'parameters' : formalParameterDecls }

def formalParameterDecls(rule):
	rule | localVariableDeclaration | formalParameterVariableElipsis
	
def formalParameterVariableElipsis(rule):
	rule | (variableModifiers, '...', variableDeclaratorId)
	rule.astAttrs = { 'modifiers' : variableModifiers , 'name' : variableDeclaratorId }

def localVariableDeclaration(rule):
	rule | (variableModifiers, _type, variableDeclaratorId)
	rule.astAttrs = { 'modifiers' : variableModifiers, 'type' : _type, 'name' : variableDeclaratorId }

def methodBody(rule):
	rule | block
	rule.astAttrs = { 'block' : block }

def constructorBody(rule):
	rule | ('{', [explicitConstructorInvocation], star(blockStatement),'}')

def explicitConstructorInvocation(rule):
	rule | ([nonWildcardTypeArguments] , _or('this' , 'super') , arguments, ';') \
	     | (primary, '.', [nonWildcardTypeArguments], 'super', arguments, ';')

def qualifiedName(rule):
	rule | (Identifier , star('.', Identifier))

def literal(rule):
	rule | NUMBER \
    |   STRING \
    |   booleanLiteral \
    |   'null' 

def integerLiteral(rule):
	rule |  NUMBER

def booleanLiteral(rule):
	rule | 'false' | 'true'

def annotations(rule):
	rule | plus(annotation)

def annotation(rule):
	rule | ('@', annotationName, ['(',_or( elementValuePairs, elementValue), ')'])

def annotationName(rule):
	rule | (Identifier, star('.', Identifier))

def elementValuePairs(rule):
	rule | commas(elementValuePair)

def elementValuePair(rule):
	rule | (Identifier, '=', elementValue)

def elementValue(rule):
	rule | conditionalExpression \
	    |   annotation \
	    |   elementValueArrayInitializer

def elementValueArrayInitializer(rule):
	rule | ( '{', [elementValue, star(',', elementValue)], [','], '}')

def annotationTypeDeclaration(rule):
	rule | ('{', star(annotationTypeElementDeclaration), '}')

def annotationTypeElementDeclaration(rule):
	rule | (modifiers, annotationTypeElementRest)

def annotationTypeElementRest(rule):
	rule | (_type, annotationMethodOrConstantRest, ';') \
	     | (normalClassDeclaration, [';']) \
	     | (normalInterfaceDeclaration, [';']) \
	     | (enumDeclaration, [';']) \
	     | (annotationTypeDeclaration, [';'])

def annotationMethodOrConstantRest(rule):
	rule |  annotationMethodRest \
    		|   annotationConstantRest

def annotationMethodRest(rule):
	rule | (Identifier, '(', ')', [defaultValue])

def annotationConstantRest(rule):
	rule | variableDeclarators

def defaultValue(rule):
	rule | ('default' , elementValue)

def block(rule):
	rule | ('{', star(blockStatement), '}')
	rule.astAttrs = { 'blockStatement' : [blockStatement] }

def blockStatement(rule):
	rule | (localVariableDeclaration, ';') \
		 | classOrInterfaceDeclaration \
		 | statement

	rule.astAttrs = { 'vardec' : localVariableDeclarationStatement,
			  'classdec' : classOrInterfaceDeclaration,
			  'statement' : statement }

def variableModifiers(rule):
	rule | star(variableModifier)

def statement(rule):
	rule | block \
	     | (ASSERT, expression, [':', expression], ';') \
         | ('if', parExpression, statement, ['else', statement]) \
		 | ('for', '(', forControl, ')', statement) \
	     | ('while', parExpression, statement) \
	     | ('do', statement, 'while', parExpression, ';') \
	     | ('try', 
			block, 
				_or(
					(catches,'finally',block), 
					catches, 
					('finally', block)
				    )
		) \
	    | ('switch', parExpression, '{', switchBlockStatementGroups, '}') \
	    | ('synchronized', parExpression, block) \
	    | ('return', [expression, ';']) \
	    | ('throw', expression, ';') \
	    | ('break', [Identifier], ';') \
	    | ('continue', [Identifier], ';') \
	    |  ';' \
	    |  (statementExpression, ';') \
	    |  (Identifier, ':', statement)

def catches(rule):
	rule | (catchClause, star(catchClause))

def catchClause(rule):
	rule | ('catch', '(', formalParameter, ')', block)

def formalParameter(rule):
	rule | (variableModifiers, _type, variableDeclaratorId)

def switchBlockStatementGroups(rule):
	rule | star(switchBlockStatementGroup)

def switchBlockStatementGroup(rule):
	rule | (plus(switchLabel), star(blockStatement))

def switchLabel(rule):
	rule | ('case', constantExpression, ':') \
	     | ('case', enumConstantName, ':') \
	     | ('default', ':')

def forControl(rule):
	rule | enhancedForControl | ([forInit], ';', [expression], ';', [forUpdate])

def forInit(rule):
	rule | localVariableDeclaration \
    |   commas(expression)

def enhancedForControl(rule):
	rule | (variableModifiers, _type, Identifier, ':', expression)

def forUpdate(rule):
	rule | commas(expression)

def parExpression(rule):
	rule | ('(', expression,')')

def statementExpression(rule):
	rule | expression

def constantExpression(rule):
	rule | expression

def expression(rule):
	rule | ( conditionalExpression, [assignmentOperator, expression])
	rule.astAttrs = { 'conditionalExpression' : conditionalExpression , 'assignmentOperator' : assignmentOperator, 'expression' : expression }

def assignmentOperator(rule):
	rule | '=' \
	    |   ('+','=') \
	    |   ('-','=') \
	    |   ('*','=') \
	    |   ('/','=') \
	    |   ('&','=') \
	    |   ('|','=') \
	    |   ('^','=') \
	    |   ('%','=') \
	    |   ('<','<','=') \
	    |   ('>','>','>','=') \
	    |   ('>','>','=') 
	rule.astAttrs = { 'value' : [ID] }

def conditionalExpression(rule):
	rule | (conditionalOrExpression, [ '?', expression, ':', expression ])
	
	rule.astAttrs = { 'condor' : conditionalOrExpression , 'cond' : [expression] }
	
def conditionalOrExpression(rule):
	rule | (conditionalAndExpression, star(('|','|'), conditionalAndExpression ))
	rule.astAttrs = { 'condand' : [conditionalAndExpression] }
	
def conditionalAndExpression(rule):
	rule | (inclusiveOrExpression, star( ('&','&') , inclusiveOrExpression ))
	rule.astAttrs = { 'inclusiveor' : [ inclusiveOrExpression ] }
	
def inclusiveOrExpression(rule):
	rule | (exclusiveOrExpression, star( '|', exclusiveOrExpression ))
	rule.astAttrs = { 'exclusiveor' : [ exclusiveOrExpression ] }
	
def exclusiveOrExpression(rule):
	rule | (andExpression, star( '^', andExpression ))
	rule.astAttrs = { 'andexpr' : [ andExpression ] }
	
def andExpression(rule):
	rule | (equalityExpression, star( '&', equalityExpression ))
	rule.astAttrs = { 'equalexpr' : [ equalityExpression ] }
	
def equalityExpression(rule):
	rule | (instanceOfExpression, star( _or(('=','=') , ('!', '=')), instanceOfExpression ))
	rule.astAttrs = { 'instanceof' : [ instanceOfExpression ] , 'comp' : [SYMBOL] }
	
def instanceOfExpression(rule):
	rule | (relationalExpression, ['instanceof', _type])
	rule.astAttrs = { 'relational' : relationalExpression, 'type' : _type }
	
def relationalExpression(rule):
	rule | (shiftExpression, star( relationalOp, shiftExpression ))
	rule.astAttrs = { 'shift' :  [shiftExpression], 'relationalOp' : [relationalOp] }
	
def relationalOp(rule):
	rule | ('<','=') | ('>','=') | ('<','>')
	rule.astAttrs = { 'op' : [SYMBOL]  }
	
def shiftExpression(rule):
	rule | (additiveExpression, star( shiftOp, additiveExpression ))
	rule.astAttrs = { 'additive' : [ additiveExpression ] , 'shift' : [shiftOp] }
	
def shiftOp(rule):
	rule | ('<','<') | ('>','>','>') | ('>','>') 
	rule.astAttrs = { 'op' : [SYMBOL] }

def additiveExpression(rule):
	rule | (multiplicativeExpression, star( _or('+' , '-'), multiplicativeExpression ))
	rule.astAttrs = { 'mulexpr' : [multiplicativeExpression], 'op' : [SYMBOL] }
	
def multiplicativeExpression(rule):
	rule | (unaryExpression, star(_or( '*' , '/' , '%' ), unaryExpression ))
	rule.astAttrs = { 'unary' : [unaryExpression] , 'op' : [SYMBOL] }

def unaryExpression(rule):
	rule | ('+', unaryExpression) \
	     | ('-', unaryExpression) \
	     | ('+','+', unaryExpression) \
	     | ('-','-', unaryExpression) \
	     | unaryExpressionNotPlusMinus
	rule.astAttrs = { 'symbol' : [SYMBOL], 'unary' : unaryExpression, 'notplusminus' : unaryExpressionNotPlusMinus }

def unaryExpressionNotPlusMinus(rule):
	rule | ('~', unaryExpression) \
    |   ('!', unaryExpression) \
    |   (castExpression) \
    |   primaryExpression
    
	rule.astAttrs = { 'symbol' : SYMBOL, 'unary' : unaryExpression, 'cast' : castExpression, 'primary' : primaryExpression }

def primaryExpression(rule):
	rule | (primary, star(selector), [_or(('+','+'),('-','-'))])
	rule.astAttrs = { 'primary' : primary, 'selector' : [selector], 'op' : [SYMBOL] }

def castExpression(rule):
	rule | ('(', primitiveType, ')', unaryExpression) \
	     | ('(', _or(_type , expression), ')', unaryExpressionNotPlusMinus)
	
	rule.astAttrs = { 'primitive' : primitiveType, 'unary' : unaryExpression, 'type' : _type, 'expr' : expression, 'unarynotplus' : unaryExpressionNotPlusMinus }
	
	
def primary(rule):
	rule | parExpression \
    |   ('this', star('.', Identifier), [identifierSuffix]) \
    |   ('super', superSuffix) \
    |   literal \
    |   ('new', creator) \
    |   (Identifier, star('.' ,Identifier), [identifierSuffix]) \
    |   (primitiveType, star('[', ']'), '.', 'class') \
    |   ('void', '.', 'class')

	rule.astAttrs = { 'parExpression' : parExpression, 
					  'superSuffix' : superSuffix, 
					  'literal' : literal, 
					  'creator' : creator,
					  'id' : [Identifier],
					  'primitive' : primitiveType,
					  'suffix' : identifierSuffix } 
    
def identifierSuffix(rule):
	rule | ( plus('[', ']'), '.', 'class') \
    |   plus('[', expression, ']') \
    |   arguments \
    |   ('.', 'class') \
    |   ('.', explicitGenericInvocation) \
    |   ('.', 'this') \
    |   ('.', 'super', arguments) \
    |   ('.', 'new', innerCreator) 
    
	rule.astAttrs = { 'ids' : [ID], 'args' : arguments, 'innerCreator' : innerCreator }

def creator(rule):
	rule | (nonWildcardTypeArguments, createdName, classCreatorRest) |   (createdName , _or(arrayCreatorRest , classCreatorRest))

def createdName(rule):
	rule | classOrInterfaceType \
    |   primitiveType

def innerCreator(rule):
	rule | ([nonWildcardTypeArguments], Identifier, classCreatorRest)

def arrayCreatorRest(rule):
    rule | ('[', _or((']', star('[',']'), arrayInitializer) ,(expression, ']', star('[', expression, ']'), star('[' , ']'))))

def classCreatorRest(rule):
	rule | (arguments , [classBody])

def explicitGenericInvocation(rule):
	rule | (nonWildcardTypeArguments, Identifier, arguments)

def nonWildcardTypeArguments(rule):
	rule | ('<', commas(_type), '>')

def selector(rule):
	rule | ('.', Identifier, [arguments]) \
	     | ('.', 'this') \
	     | ('.', 'super', superSuffix) \
	     | ('.', 'new', innerCreator) \
	     | ('[', expression, ']')

def superSuffix(rule):
	rule | arguments | ('.', Identifier, [arguments])

def arguments(rule):
	rule | ('(', [commas(expression)], ')')


def ASSERT(rule):
	rule | 'assert'

def ENUM(rule):
	rule | 'enum'

def Identifier(rule):
	rule | ID
	rule.astAttrs = { 'id' : ID }

class SYMBOL(CharToken):
    chars = '{},[]:.;()=|+*'
    num = len(chars)

java_grammar = Grammar(start=compilationUnit,
                  tokens=[SYMBOL],
                  ignore=[WHITE, NEWLINE],
                  ast_tokens=[NUMBER, ID])

