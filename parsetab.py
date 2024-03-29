
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = "left!ANDORLEQHEQNOTEQEQUALS<>left=:left+-left*/left()[]{}AND COMMENT DEF ELSE EQUALS EXIT FOR HEQ ID IDEN IF INT LENGTH LEQ LIST NOTEQ NULL NUMBER OR PRINT RETURN SCAN STR STRING TO VAR VECTOR WHILEprog :\n            | func progfunc : DEF type iden '(' flist ')' '{' body '}'\n            | DEF type iden '(' flist ')' RETURN expr ';'body : \n            | stmt bodystmt : expr ';'\n            | defvar ';'\n            | IF '(' expr ')' stmt\n            | IF '(' expr ')' stmt ELSE stmt\n            | WHILE '(' expr ')' stmt\n            | FOR '(' iden '=' expr TO expr ')' stmt \n            | RETURN expr ';'\n            | '{' body '}'\n            | funcdefvar : VAR type iden\n              | VAR type iden '=' exprflist :\n            | type iden\n            | type iden ',' flistclist :\n            | expr\n            | expr ',' clistexpr : expr '[' expr ']'\n            | '[' clist ']' \n            | expr '?' expr ':' expr \n            | expr '+' expr \n            | expr '-' expr \n            | expr '*' expr \n            | expr '/' expr \n            | expr '%' expr \n            | expr '>' expr \n            | expr '<' expr \n            | expr EQUALS expr \n            | expr HEQ expr \n            | expr LEQ  expr \n            | expr NOTEQ expr \n            | expr OR expr \n            | expr AND expr \n            | '!' expr \n            | '+' expr \n            | '-' expr \n            | iden \n            | iden '=' expr \n            | iden '(' clist ')' \n            | num\n            | str iden : IDEN num : NUMBER  str : STR  type : INT\n            | NULL\n            | VECTOR"
    
_lr_action_items = {'$end':([0,1,2,4,44,73,],[-1,0,-1,-2,-3,-4,]),'DEF':([0,2,17,21,23,30,44,46,62,73,76,95,102,103,108,109,112,114,116,117,],[3,3,3,3,3,-15,-3,-7,-8,-4,-14,-13,3,3,-9,-11,3,-10,3,-12,]),'INT':([3,11,16,37,],[6,6,6,6,]),'NULL':([3,11,16,37,],[7,7,7,7,]),'VECTOR':([3,11,16,37,],[8,8,8,8,]),'IDEN':([5,6,7,8,12,17,18,21,23,29,30,31,32,33,34,41,42,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,72,73,76,95,97,101,102,103,104,106,108,109,112,113,114,116,117,],[10,-51,-52,-53,10,10,10,10,10,10,-15,10,10,10,10,10,10,-3,-7,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,-8,10,10,10,10,-4,-14,-13,10,10,10,10,10,10,-9,-11,10,10,-10,10,-12,]),'(':([9,10,20,26,27,28,],[11,-48,42,63,64,65,]),',':([10,14,20,35,36,38,39,68,69,70,71,74,79,80,81,82,83,84,85,86,87,88,89,90,91,96,99,100,107,],[-48,16,-43,-46,-47,-49,-50,97,-41,-42,-40,-44,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-25,-45,-24,-26,]),')':([10,11,13,14,16,19,20,35,36,38,39,42,68,69,70,71,74,75,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,97,99,100,105,107,115,],[-48,-18,15,-19,-18,-20,-43,-46,-47,-49,-50,-21,-22,-41,-42,-40,-44,99,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,102,103,-25,-21,-45,-24,-23,-26,116,]),'=':([10,20,94,98,],[-48,41,104,106,]),';':([10,20,24,25,35,36,38,39,40,66,69,70,71,74,79,80,81,82,83,84,85,86,87,88,89,90,91,96,98,99,100,107,111,],[-48,-43,46,62,-46,-47,-49,-50,73,95,-41,-42,-40,-44,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-25,-16,-45,-24,-26,-17,]),'[':([10,17,18,20,21,23,24,29,30,31,32,33,34,35,36,38,39,40,41,42,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,70,71,73,74,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,95,96,97,99,100,101,102,103,104,106,107,108,109,110,111,112,113,114,115,116,117,],[-48,31,31,-43,31,31,47,31,-15,31,31,31,31,-46,-47,-49,-50,47,31,31,-3,-7,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,-8,31,31,47,47,47,47,47,-4,47,-14,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,-13,-25,31,-45,-24,31,31,31,31,31,47,-9,-11,47,47,31,31,-10,47,31,-12,]),'?':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,48,-46,-47,-49,-50,48,48,48,-41,-42,-40,-44,48,48,-27,-28,-29,-30,48,-32,-33,-34,-35,-36,-37,-38,-39,48,48,-25,-45,-24,-26,48,48,48,]),'+':([10,17,18,20,21,23,24,29,30,31,32,33,34,35,36,38,39,40,41,42,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,70,71,73,74,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,95,96,97,99,100,101,102,103,104,106,107,108,109,110,111,112,113,114,115,116,117,],[-48,32,32,-43,32,32,49,32,-15,32,32,32,32,-46,-47,-49,-50,49,32,32,-3,-7,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,-8,32,32,49,49,-41,-42,49,-4,49,-14,49,49,-27,-28,-29,-30,49,49,49,49,49,49,49,49,49,49,49,-13,-25,32,-45,-24,32,32,32,32,32,49,-9,-11,49,49,32,32,-10,49,32,-12,]),'-':([10,17,18,20,21,23,24,29,30,31,32,33,34,35,36,38,39,40,41,42,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,70,71,73,74,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,95,96,97,99,100,101,102,103,104,106,107,108,109,110,111,112,113,114,115,116,117,],[-48,33,33,-43,33,33,50,33,-15,33,33,33,33,-46,-47,-49,-50,50,33,33,-3,-7,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,-8,33,33,50,50,-41,-42,50,-4,50,-14,50,50,-27,-28,-29,-30,50,50,50,50,50,50,50,50,50,50,50,-13,-25,33,-45,-24,33,33,33,33,33,50,-9,-11,50,50,33,33,-10,50,33,-12,]),'*':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,51,-46,-47,-49,-50,51,51,51,51,51,51,51,51,51,51,51,-29,-30,51,51,51,51,51,51,51,51,51,51,51,-25,-45,-24,51,51,51,51,]),'/':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,52,-46,-47,-49,-50,52,52,52,52,52,52,52,52,52,52,52,-29,-30,52,52,52,52,52,52,52,52,52,52,52,-25,-45,-24,52,52,52,52,]),'%':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,53,-46,-47,-49,-50,53,53,53,-41,-42,-40,-44,53,53,-27,-28,-29,-30,53,-32,-33,-34,-35,-36,-37,-38,-39,53,53,-25,-45,-24,-26,53,53,53,]),'>':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,54,-46,-47,-49,-50,54,54,54,-41,-42,-40,-44,54,54,-27,-28,-29,-30,54,-32,-33,-34,-35,-36,-37,-38,-39,54,54,-25,-45,-24,-26,54,54,54,]),'<':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,55,-46,-47,-49,-50,55,55,55,-41,-42,-40,-44,55,55,-27,-28,-29,-30,55,-32,-33,-34,-35,-36,-37,-38,-39,55,55,-25,-45,-24,-26,55,55,55,]),'EQUALS':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,56,-46,-47,-49,-50,56,56,56,-41,-42,-40,-44,56,56,-27,-28,-29,-30,56,-32,-33,-34,-35,-36,-37,-38,-39,56,56,-25,-45,-24,-26,56,56,56,]),'HEQ':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,57,-46,-47,-49,-50,57,57,57,-41,-42,-40,-44,57,57,-27,-28,-29,-30,57,-32,-33,-34,-35,-36,-37,-38,-39,57,57,-25,-45,-24,-26,57,57,57,]),'LEQ':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,58,-46,-47,-49,-50,58,58,58,-41,-42,-40,-44,58,58,-27,-28,-29,-30,58,-32,-33,-34,-35,-36,-37,-38,-39,58,58,-25,-45,-24,-26,58,58,58,]),'NOTEQ':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,59,-46,-47,-49,-50,59,59,59,-41,-42,-40,-44,59,59,-27,-28,-29,-30,59,-32,-33,-34,-35,-36,-37,-38,-39,59,59,-25,-45,-24,-26,59,59,59,]),'OR':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,60,-46,-47,-49,-50,60,60,60,-41,-42,-40,-44,60,60,-27,-28,-29,-30,60,-32,-33,-34,-35,-36,-37,-38,-39,60,60,-25,-45,-24,-26,60,60,60,]),'AND':([10,20,24,35,36,38,39,40,66,68,69,70,71,74,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,96,99,100,107,110,111,115,],[-48,-43,61,-46,-47,-49,-50,61,61,61,-41,-42,-40,-44,61,61,-27,-28,-29,-30,61,-32,-33,-34,-35,-36,-37,-38,-39,61,61,-25,-45,-24,-26,61,61,61,]),']':([10,20,31,35,36,38,39,67,68,69,70,71,74,77,79,80,81,82,83,84,85,86,87,88,89,90,91,96,97,99,100,105,107,],[-48,-43,-21,-46,-47,-49,-50,96,-22,-41,-42,-40,-44,100,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-25,-21,-45,-24,-23,-26,]),':':([10,20,35,36,38,39,69,70,71,74,78,79,80,81,82,83,84,85,86,87,88,89,90,91,96,99,100,107,],[-48,-43,-46,-47,-49,-50,-41,-42,-40,-44,101,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-25,-45,-24,-26,]),'TO':([10,20,35,36,38,39,69,70,71,74,79,80,81,82,83,84,85,86,87,88,89,90,91,96,99,100,107,110,],[-48,-43,-46,-47,-49,-50,-41,-42,-40,-44,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-25,-45,-24,-26,113,]),'{':([15,17,21,23,30,44,46,62,73,76,95,102,103,108,109,112,114,116,117,],[17,21,21,21,-15,-3,-7,-8,-4,-14,-13,21,21,-9,-11,21,-10,21,-12,]),'RETURN':([15,17,21,23,30,44,46,62,73,76,95,102,103,108,109,112,114,116,117,],[18,29,29,29,-15,-3,-7,-8,-4,-14,-13,29,29,-9,-11,29,-10,29,-12,]),'}':([17,21,22,23,30,43,44,45,46,62,73,76,95,108,109,114,117,],[-5,-5,44,-5,-15,76,-3,-6,-7,-8,-4,-14,-13,-9,-11,-10,-12,]),'IF':([17,21,23,30,44,46,62,73,76,95,102,103,108,109,112,114,116,117,],[26,26,26,-15,-3,-7,-8,-4,-14,-13,26,26,-9,-11,26,-10,26,-12,]),'WHILE':([17,21,23,30,44,46,62,73,76,95,102,103,108,109,112,114,116,117,],[27,27,27,-15,-3,-7,-8,-4,-14,-13,27,27,-9,-11,27,-10,27,-12,]),'FOR':([17,21,23,30,44,46,62,73,76,95,102,103,108,109,112,114,116,117,],[28,28,28,-15,-3,-7,-8,-4,-14,-13,28,28,-9,-11,28,-10,28,-12,]),'!':([17,18,21,23,29,30,31,32,33,34,41,42,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,73,76,95,97,101,102,103,104,106,108,109,112,113,114,116,117,],[34,34,34,34,34,-15,34,34,34,34,34,34,-3,-7,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,-8,34,34,-4,-14,-13,34,34,34,34,34,34,-9,-11,34,34,-10,34,-12,]),'VAR':([17,21,23,30,44,46,62,73,76,95,102,103,108,109,112,114,116,117,],[37,37,37,-15,-3,-7,-8,-4,-14,-13,37,37,-9,-11,37,-10,37,-12,]),'NUMBER':([17,18,21,23,29,30,31,32,33,34,41,42,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,73,76,95,97,101,102,103,104,106,108,109,112,113,114,116,117,],[38,38,38,38,38,-15,38,38,38,38,38,38,-3,-7,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,-8,38,38,-4,-14,-13,38,38,38,38,38,38,-9,-11,38,38,-10,38,-12,]),'STR':([17,18,21,23,29,30,31,32,33,34,41,42,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,73,76,95,97,101,102,103,104,106,108,109,112,113,114,116,117,],[39,39,39,39,39,-15,39,39,39,39,39,39,-3,-7,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,-8,39,39,-4,-14,-13,39,39,39,39,39,39,-9,-11,39,39,-10,39,-12,]),'ELSE':([30,44,46,62,73,76,95,108,109,114,117,],[-15,-3,-7,-8,-4,-14,-13,-9,-11,-10,-12,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'prog':([0,2,],[1,4,]),'func':([0,2,17,21,23,102,103,112,116,],[2,2,30,30,30,30,30,30,30,]),'type':([3,11,16,37,],[5,12,12,72,]),'iden':([5,12,17,18,21,23,29,31,32,33,34,41,42,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,72,97,101,102,103,104,106,112,113,116,],[9,14,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,94,98,20,20,20,20,20,20,20,20,20,]),'flist':([11,16,],[13,19,]),'body':([17,21,23,],[22,43,45,]),'stmt':([17,21,23,102,103,112,116,],[23,23,23,108,109,114,117,]),'expr':([17,18,21,23,29,31,32,33,34,41,42,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,97,101,102,103,104,106,112,113,116,],[24,40,24,24,66,68,69,70,71,74,68,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,68,107,24,24,110,111,24,115,24,]),'defvar':([17,21,23,102,103,112,116,],[25,25,25,25,25,25,25,]),'num':([17,18,21,23,29,31,32,33,34,41,42,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,97,101,102,103,104,106,112,113,116,],[35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,]),'str':([17,18,21,23,29,31,32,33,34,41,42,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,97,101,102,103,104,106,112,113,116,],[36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,]),'clist':([31,42,97,],[67,75,105,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> prog","S'",1,None,None,None),
  ('prog -> <empty>','prog',0,'p_prog','compiler.py',131),
  ('prog -> func prog','prog',2,'p_prog','compiler.py',132),
  ('func -> DEF type iden ( flist ) { body }','func',9,'p_func','compiler.py',137),
  ('func -> DEF type iden ( flist ) RETURN expr ;','func',9,'p_func','compiler.py',138),
  ('body -> <empty>','body',0,'p_body','compiler.py',167),
  ('body -> stmt body','body',2,'p_body','compiler.py',168),
  ('stmt -> expr ;','stmt',2,'p_stmt','compiler.py',174),
  ('stmt -> defvar ;','stmt',2,'p_stmt','compiler.py',175),
  ('stmt -> IF ( expr ) stmt','stmt',5,'p_stmt','compiler.py',176),
  ('stmt -> IF ( expr ) stmt ELSE stmt','stmt',7,'p_stmt','compiler.py',177),
  ('stmt -> WHILE ( expr ) stmt','stmt',5,'p_stmt','compiler.py',178),
  ('stmt -> FOR ( iden = expr TO expr ) stmt','stmt',9,'p_stmt','compiler.py',179),
  ('stmt -> RETURN expr ;','stmt',3,'p_stmt','compiler.py',180),
  ('stmt -> { body }','stmt',3,'p_stmt','compiler.py',181),
  ('stmt -> func','stmt',1,'p_stmt','compiler.py',182),
  ('defvar -> VAR type iden','defvar',3,'p_defvar','compiler.py',215),
  ('defvar -> VAR type iden = expr','defvar',5,'p_defvar','compiler.py',216),
  ('flist -> <empty>','flist',0,'p_flist','compiler.py',233),
  ('flist -> type iden','flist',2,'p_flist','compiler.py',234),
  ('flist -> type iden , flist','flist',4,'p_flist','compiler.py',235),
  ('clist -> <empty>','clist',0,'p_clist','compiler.py',251),
  ('clist -> expr','clist',1,'p_clist','compiler.py',252),
  ('clist -> expr , clist','clist',3,'p_clist','compiler.py',253),
  ('expr -> expr [ expr ]','expr',4,'p_expr','compiler.py',265),
  ('expr -> [ clist ]','expr',3,'p_expr','compiler.py',266),
  ('expr -> expr ? expr : expr','expr',5,'p_expr','compiler.py',267),
  ('expr -> expr + expr','expr',3,'p_expr','compiler.py',268),
  ('expr -> expr - expr','expr',3,'p_expr','compiler.py',269),
  ('expr -> expr * expr','expr',3,'p_expr','compiler.py',270),
  ('expr -> expr / expr','expr',3,'p_expr','compiler.py',271),
  ('expr -> expr % expr','expr',3,'p_expr','compiler.py',272),
  ('expr -> expr > expr','expr',3,'p_expr','compiler.py',273),
  ('expr -> expr < expr','expr',3,'p_expr','compiler.py',274),
  ('expr -> expr EQUALS expr','expr',3,'p_expr','compiler.py',275),
  ('expr -> expr HEQ expr','expr',3,'p_expr','compiler.py',276),
  ('expr -> expr LEQ expr','expr',3,'p_expr','compiler.py',277),
  ('expr -> expr NOTEQ expr','expr',3,'p_expr','compiler.py',278),
  ('expr -> expr OR expr','expr',3,'p_expr','compiler.py',279),
  ('expr -> expr AND expr','expr',3,'p_expr','compiler.py',280),
  ('expr -> ! expr','expr',2,'p_expr','compiler.py',281),
  ('expr -> + expr','expr',2,'p_expr','compiler.py',282),
  ('expr -> - expr','expr',2,'p_expr','compiler.py',283),
  ('expr -> iden','expr',1,'p_expr','compiler.py',284),
  ('expr -> iden = expr','expr',3,'p_expr','compiler.py',285),
  ('expr -> iden ( clist )','expr',4,'p_expr','compiler.py',286),
  ('expr -> num','expr',1,'p_expr','compiler.py',287),
  ('expr -> str','expr',1,'p_expr','compiler.py',288),
  ('iden -> IDEN','iden',1,'p_iden','compiler.py',340),
  ('num -> NUMBER','num',1,'p_num','compiler.py',346),
  ('str -> STR','str',1,'p_str','compiler.py',352),
  ('type -> INT','type',1,'p_type','compiler.py',365),
  ('type -> NULL','type',1,'p_type','compiler.py',366),
  ('type -> VECTOR','type',1,'p_type','compiler.py',367),
]
