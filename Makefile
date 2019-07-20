##
## EPITECH PROJECT, 2019
## haskell
## File description:
## haskell
##

NAME	=	imageCompressor

SRC	=	app/Main.hs	\

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .

clean:
	stack clean
	rm -rf .stack-work imageCompressor.cabal ChangeLog.md

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
