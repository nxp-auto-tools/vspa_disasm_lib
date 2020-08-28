DEFS=

CXX=g++
CXXFLAGS=-DISS
OBJDIR=bin
SRCDIR=src
INCDIR=src/h
AR=ar
RANLIB=ranlib

all: $(OBJDIR)/libdisasm.so  $(OBJDIR)/libdisasm.a

$(OBJDIR)/libdisasm.so: $(OBJDIR)/ruby_dis.o $(OBJDIR)/ruby_op.o
	$(CXX) $(CXXFLAGS) $(DEFS) -shared  $^ -o $@
	
$(OBJDIR)/libdisasm.a: $(OBJDIR)/ruby_dis.o $(OBJDIR)/ruby_op.o
	$(AR) cru $@ $^
	$(RANLIB) $@

$(OBJDIR)/ruby_dis.o: $(SRCDIR)/ruby_dis.cpp $(OBJDIR)
	$(CXX) $(CXXFLAGS) $(DEFS) -I$(INCDIR) -c $< -o $@

$(OBJDIR)/ruby_op.o: ../sc3900/SC3900DA/vspa.gen.cpp $(OBJDIR)
	$(CXX) $(CXXFLAGS) $(DEFS) -I$(INCDIR) -c $< -o $@

$(OBJDIR):
	mkdir -p $@

clean:
	rm -rf bin
