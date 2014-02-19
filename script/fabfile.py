from fabric.api import *
import sys
from os import environ
import subprocess

env.use_ssh_config = True

execfile('hosts.py')

l = len(env.hosts)
print "Running on %s hosts." % l

@parallel
def plan ():
    run ("cd " + environ["PWD"] + "; sbcl --load script.lisp")

def dryrun ():
    run ("cd " + environ["PWD"] + "; cat script.lisp")


def echo ():
    run ("echo echo")

def w ():
    run ("w")

def free ():
    run ("free")

def pwd ():
    run ("echo "+ environ["PWD"])

def pstree ():
    run ("pstree")


def grepsbcl ():
    run ("pgrep sbcl; exit 0")

def killsbcl ():
    run ("pkill sbcl; exit 0")

def hostname ():
    run ("hostname")

