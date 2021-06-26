#My zshrc file

source ~/.profile # Source environmental variables from .profile

function cleanmega # Delete the mega.nz cache in Opera
{
    rm -r '/Users/Harvey/Library/Application Support/com.operasoftware.Opera/File System' && echo 'All Done.'
}

# Coding stuff
export PATH="/usr/local/opt/ruby/bin:/usr/local/lib/ruby/gems/2.7.0/bin:$PATH"
alias emacs='emacs -nw'
alias emacsclient="/usr/local/Cellar/emacs-plus\\@27/27.2/bin/emacsclient" # Use the emacs-plus emacsclient binary
alias ecli='emacsclient -n'
# For LLVM
LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
export PATH="/usr/local/opt/llvm/bin:$PATH"
#export LDFLAGS="-L/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"

export GOPATH="/Users/Harvey/go"
export PATH="$(go env GOPATH)/bin:$PATH"
export EDITOR='emacs'
export TERM=xterm-256color
alias jav='~/bin/jav'

function crun # Build and run a .c or .cpp file
{
    [[ $# == 1 ]] || [[ $# == 2 ]] || { echo "crun: usage: crun [-w] [file]" && return 1 }
    # -w flag was passed
    [[ $# == 2 ]] && { { [[ $1 == "-w" ]] || { echo "crun: unknown flag $1" && return 1 } } && { [[ $2 == *.cpp ]] && c++ -Weverything -std=c++17 $2 && ./a.out } || { [[ $2 == *.c ]] && cc -Weverything $2 && ./a.out } || { echo "crun: run failed" && return 1 } } && return 0
    # No flag
    { [[ $1 == *.cpp ]] && c++ -std=c++17 $1 && ./a.out } || { [[ $1 == *.c ]] && cc $1 && ./a.out } || { echo "crun: run failed" && return 1 }
}

function cdb # Build and debug a .c or .cpp file
{
    [ $# -eq 1 ] || { echo "cdb: usage: cdb [file]"; return 1 }
    { [[ $1 = *.cpp ]] && c++ -g $1 && lldb ./a.out } || { [[ $1 = *.c ]] && cc -g $1 && lldb ./a.out } || echo "cdb: run failed"
}

function cleanlogs # Delete terminal log files
{
    sudo rm /private/var/log/asl/*.asl
}

function jbuild # Build and jar java source files
{ # Pass the name (without .java extension) of the file containing main driver (ie. the manifest)
    [ $# -eq 1 ] || { echo "jbuild: usage: jbuild [manifest]"; return 1 }
    [ -d "./build" ] || mkdir build # Make the build directory if it is not present
    javac -d ./build *.java && { cd build; jar -cvfe $1.jar $1 *.class && mv $1.jar ../$1.jar; cd .. }
}

# Macros to help manage jav collection

function vrscale # Scale a VR video to 2880p
{
    [[ $# != 0 ]] || { echo "vrscale: usage: vrscale [file ...]"; return 1 }
    for file in $@; do ffmpeg -i $file -vf scale=2880:-1 $(basename $file .mp4)-scaled.mp4 && rm $file; done
}

function hjdtrim # Trim out ad intros in jav videos
{
    [[ $# > 1 ]] || { echo "hdjtrim: usage: hdjtrim [short/long] [file ...]"; return 1 }
    { [[ $1 == "long" ]] && { shift; for file in $@; do { ffmpeg -y -i $file -ss 00:01:52 -c copy $file-trimmed.mp4 && rm $file }; done } } ||
        { [[ $1 == "short" ]] && { shift; for file in $@; do { ffmpeg -y -i $file -ss 00:01:02 -c copy $file-trimmed.mp4 && rm $file }; done } } ||
        { [[ $1 == "yabo" ]] && { shift; for file in $@; do { ffmpeg -y -i $file -ss 00:01:23 -c copy $file-trimmed.mp4 && rm $file }; done} } ||
        { [[ $1 == "longlong" ]] && { shift; for file in $@; do { ffmpeg -y -i $file -ss 00:02:25 -c copy $file-trimmed.mp4 && rm $file }; done } } ||
        { [[ $1 == "superlong" ]] && { shift; for file in $@; do { ffmpeg -y -i $file -ss 00:03:40 -c copy $file-trimmed.mp4 && rm $file }; done } } ||
        { echo "hjdtrim: unknown command"; return 1 }
}

function javlink # Create symlinks for all jav files given and place them in the specified directory
{
    [[ $# > 1 ]] || { echo "javlink: usage: javlink [destination] [file ...]"; return 1 }
    dest=$1
    [ -d $dest ] || mkdir $dest
    shift
    for file in $@; do { filename=`basename $file`; fileext=${filename##*.}; ln -sf $(pwd)/$file $dest/$(jav -o title $file).$fileext }; done
}

function mvtodir # Move all specified files to a specified directory
{
    [[ $# > 1 ]] || { echo "mvtodir: usage: mvtodir [destination] [file ...]"; return 1 }
    dest=$1
    [ -d $dest ] || mkdir $dest
    shift
    for file in $@; do { mv $file $dest/$file }; done
}

function cptodir # Copy all specified files to a specified directory
{
    [[ $# > 1 ]] || { echo "cptodir: usage: cptodir [destination] [file ...]"; return 1 }
    dest=$1
    [ -d $dest ] || mkdir $dest
    shift
    for file in $@; do { cp $file $dest/$file }; done
}

# Plug-ins
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # Enable syntax-highlighting
source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh # Enable auto-suggestions
