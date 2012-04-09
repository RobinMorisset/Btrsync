import sys
import subprocess
import os

subprocess.check_call(["tar","vcf",sys.argv[1]+"tmp",sys.argv[1]])
p = subprocess.Popen(["ssh "+"clipper.ens.fr "+"tar "+"vcf "+sys.argv[2]+"tmp "+sys.argv[2]+"/"+os.path.basename(sys.argv[1])],shell=True)
p.communicate()
subprocess.check_call(["rsync","-a",sys.argv[1]+"tmp","clipper.ens.fr:"+sys.argv[2]+"tmp"])
l = subprocess.Popen(["ssh "+"clipper.ens.fr "+"tar "+"vxf "+sys.argv[2]+"tmp "+"-C "+sys.argv[2]],shell=True)
l.communicate()
subprocess.check_call(["rm","-f",sys.argv[1]+"tmp"])
s = subprocess.Popen(["ssh "+"clipper.ens.fr "+"rm "+"-f "+sys.argv[2]+"tmp"],shell=True)
s.communicate()
