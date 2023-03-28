!compiler_flags: --dump LEX --exec LEX
-- prva vrstica v datoteki je namenjena
-- konfiguraciji prevajalnika

-- -------------------------------------

-- struktura 'delujočega' testa
-- !name: -- značka !name ni obvezna
-- !code:
-- 
-- !expected:
-- 
-- !end

-- struktura 'nedelujočega' testa
-- !name:
-- !code:
-- 
-- !failure:
-- 99 -- izhodna koda programa
-- !end

-- ------------------------------------

-- 'javni testi' za leksikalno analizo

!code:
10+0020
!expected:
[1:1-1:3] C_INTEGER:10
[1:3-1:4] OP_ADD:+
[1:4-1:8] C_INTEGER:0020
EOF:$
!end

!code:
while != { if true
!expected:
[1:1-1:6] KW_WHILE:while
[1:7-1:9] OP_NEQ:!=
[1:10-1:11] OP_LBRACE:{
[1:12-1:14] KW_IF:if
[1:15-1:19] C_LOGICAL:true
EOF:$
!end

!code:
    'danes je ''lep'' dan'
!expected:
[1:5-1:27] C_STRING:danes je 'lep' dan
EOF:$
!end

!code:
true

  <=&

     false
!expected:
[1:1-1:5] C_LOGICAL:true
[3:3-3:5] OP_LEQ:<=
[3:5-3:6] OP_AND:&
[5:6-5:11] C_LOGICAL:false
EOF:$
!end

!code:
'
!failure:
99
!end
