#!/usr/bin/env python3

from bs4 import BeautifulSoup

operands = {
    'imm': 'Operand.IMMEDIATE',
    'zp':  'Operand.ZEROPAGE',
    'zpx': 'Operand.ZEROPAGE_X',
    'zpy': 'Operand.ZEROPAGE_Y',
    'izx': 'Operand.INDIRECT_X',
    'izy': 'Operand.INDIRECT_Y',
    'abs': 'Operand.ABSOLUTE',
    'abx': 'Operand.ABSOLUTE_X',
    'aby': 'Operand.ABSOLUTE_Y',
    'ind': 'Operand.INDIRECT',
    'rel': 'Operand.RELATIVE',
    ''   : 'Operand.XXX /* This need manual editing */'
}

f = open( 'opcodes2.html', 'rt' )
d = f.read( )
f.close( )

bs = BeautifulSoup( d, 'lxml' )
trs = bs.find( 'table' ).find_all( 'tr' )[ 1: ]

output = []
FORMAT_STRING = 'Opcode( Operation.{}, {}, {} ),'
opcodes = set()

for i, tr in enumerate( trs ):
    tds = tr.find_all( 'td' )[ 1: ]

    for j, td in enumerate( tds ):
        t = td.getText( '|' )
        if t == 'KIL':
            print( 'null,' )
        else:
            opcode, cycles = td.getText( '|' ).split( '|' )
            opcodes.add( opcode )
            operand = ''
            cycles = cycles.replace( '*', '' )

            if not cycles.isdigit():
                operand, cycles = cycles.split( ' ' )

            print( FORMAT_STRING.format( opcode, operands[ operand ], cycles ) + ( '', '\t// 0x%.2x' % ( ( i << 4 ) | j ) )[ not operand ] )

    print( '' )

print( '\n' )
last = 'A'

opcodes = sorted( opcodes )

for opcode in opcodes:
    if opcode[ 0 ] != last:
        last = opcode[ 0 ]
        print( '' )

    print( opcode, end = ', ' )

print( '\n' )

for opcode in opcodes:
    print( '[OperationHandler( Operation.{} )]'.format( opcode ) )
    print( "public void handler{}( Operand type ) {{\n\tthrow \"{} is not yet implemented.\";\n}}\n".format( opcode, opcode ) )
