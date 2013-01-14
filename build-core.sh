#!/bin/sh
##
## build-core.sh
## 
## Made by  Strandh Robert
## Login   <rostrand@mcgonagall.emi.u-bordeaux1.fr>
## 
## Started on  Tue Dec 18 16:26:19 2007  Strandh Robert
## Last update Mon Mar  1 16:23:39 2010 Antoine Lucas
##
export SBCL_HOME=/opt/local/lib/sbcl
sbcl --no-userinit <<EOF
(require 'asdf)
(push "/net/cremi/alucas/projetProgramation4/clbuild/systems/" asdf:*central-registry*)
(loop for system in (list '#:spatial-trees
			  '#:split-sequence
			  '#:flexichain
			  '#:clx
			  '#:mcclim)
      do (asdf:operate 'asdf:load-op system))

(save-lisp-and-die "clim.core")
EOF
