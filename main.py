#!/usr/bin/python
import grammar
import translator
import sys

if __name__ == '__main__':
	f = open(sys.argv[1], 'r')
	data = f.read()

	translated = translator.Java.from_string(data)

	for cls in translated.classes:
		print 'methods: ', cls.methods
		print 'class name: ', cls.name
	
