#!/usr/bin/env zsh
# Write git commit hash into documentation.

# Bypass interactive questioning if any argument is passed in.
say_yes=""
if [[ $# -gt 0 ]]; then
    say_yes="y"
fi

# Give warning if the git repository is unclean in any way (there are either
# staged or unstaged changes).
if [[ $(git status --porcelain -uno | wc -l) -gt 0 ]]; then
    echo "[WARNING] Some tracked files are modified:"
    git status -s -uno
    if [[ -z $say_yes ]]; then
        while true; do
            read 'reply?Continue anyway? (y/n): '
            case $reply in
                [Yy]) break ;;
                [Nn]) exit 0 ;;
                *) printf '%s\n' 'Please answer y or n.' ;;
            esac
        done
    fi
fi

# Find latest commit hash.
hash=$(git rev-list --all -1)

# Replace "AUCA_VERSION_TEXT" line in panxor.tex with $hash.
cat panxor.tex | sed \
    -e "s/AUCA_VERSION_TEXT/$hash/"\
    > panxor-versioned.tex

# Remove old versions.
rm -f panxor-*.pdf

# Compile PDF.
make

# Append $hash to filename as well.
cp -f panxor-versioned.pdf panxor-$hash.pdf
