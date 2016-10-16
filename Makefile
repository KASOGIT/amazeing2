##
## Makefile for  in /home/kaso/Documents/rendu/EPITECH/YEAR2/PROJECT/OCAML_2015_amazeing1
## 
## Made by 
## Login   <@epitech.net>
## 
## Started on  Sat Apr  2 16:42:21 2016 
## Last update Sun May  1 01:04:09 2016 Kévin Julien
##

all:
	$(MAKE) -C ./step3
	$(MAKE) -C ./step4

byte:
	$(MAKE) -C ./step3 byte
	$(MAKE) -C ./step4 byte

clean:
	$(MAKE) -C ./step3/ clean
	$(MAKE) -C ./step4/ clean

fclean:
	$(MAKE) -C ./step3/ fclean
	$(MAKE) -C ./step4/ fclean

re:
	$(MAKE) -C ./step3/ re
	$(MAKE) -C ./step4/ re
