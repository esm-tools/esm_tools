# Create a folder where you want to install different Perls, and cd into it:
# Note that it doesn't need to be your home folder. Put it wherever you want to maintain such software:
version="5.32.0"
export PERL_BASE="$HOME/perl"
mkdir -p $PERL_BASE
cd $PERL_BASE

# Download source tarball into a subfolder named src, and untar:
curl --create-dirs -L -o src/perl-${version}.tar.gz http://www.cpan.org/src/5.0/perl-${version}.tar.gz
cd src
tar -zxf perl-${version}.tar.gz

# Configure and build, making sure that this non-standard location is baked into the perl binary:
cd perl-${version}
./Configure -des -Dprefix=$PERL_BASE/perl-${version} -Dotherlibdirs=$PERL_BASE/perl-${version}/lib/perl5
make
make install

# Install modules needed for OpenIFS / XIOS
curl -sL https://cpanmin.us | $PERL_BASE/perl-${version}/bin/perl - --notest -l $PERL_BASE/perl-${version} App::cpanminus Time::Piece URI

cat >> ~/.bash_profile << EOF

# Set \$PERL5LIB to find these libraries, and set \$PATH to use this Perl instead of the system Perl:
export PERL_BASE=\$HOME/perl
export PERL5LIB=\$PERL_BASE/perl-${version}/lib/perl5:\$PERL_BASE/perl-${version}/lib/perl5/x86_64-linux:\$PERL5LIB
export PATH=\$PERL_BASE/perl-${version}/bin:\$PATH
EOF
