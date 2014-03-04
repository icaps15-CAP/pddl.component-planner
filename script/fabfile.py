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
    print "On host %d" % env.hosts.index(env.host)
    run ("cd " + environ["PWD"] + "; ./lispimage %d %d" % (l, env.hosts.index(env.host)))

def dryrun ():
    print "On host %d" % env.hosts.index(env.host)
    run ("cd " + environ["PWD"] + "; echo ./lispimage %d %d" % (l, env.hosts.index(env.host)))
    run ("cd " + environ["PWD"] + "; ./lispimage -d %d %d" % (l, env.hosts.index(env.host)))


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

