export PS1='%F{cyan}%B%n%b%f%B%F{magenta} %f%b%B%F{yellow}%m%f%b %F{green}%B%~%b%f %b%D{%n}%F{green}%B>%b%f%F{white} '
alias cgrep="grep --color=always"

export PATH="$HOME/.npm-global/bin:$PATH"
export PATH="$PATH:/home/eldriv/.local/share/gem/ruby/3.3.0/bin"
export PATH="$HOME/script:$HOME/.emacs.d/bin:$HOME/bin:$HOME/bin:$HOME/src/clingon-cli/cli-ems:$PATH"

# Display list of directories and files as well as hidden files.
function l {
  eza -s modified -Ah "$@"
}

function ch {
  duf
}

function bth {
  inxi --battery
}

# Display list of directories with their permissions 
function ll {
  l -lO "$@"
}

# Function to open the NixOS configuration
function root-nixos-config {
  sudo vim /etc/nixos/configuration.nix
}

# Function to search the definition inside maries' source file
function s-marie() {
    cd ~/common-lisp/marie/src || return
    local term="$1"
    if [ -z "$term" ]; then
        echo "Usage: s-marie <term> must have a value"
        return 1
    fi
    grep -ri "$term"
}

# Convert a directory to tarball
function tar! {
  local date=$(date +"%Y%m%d%H%M")
  local tarball=${date}-${1}.tar.gz

  tar czf "${tarball}" "$1"
}

# Git commands
function git {
  local git= self= op=

  if [[ -n "${BASH}" ]]; then
    git=$(which git)
    self=${FUNCNAME}
  elif [[ -n "${ZSH_NAME}" ]]; then
    git=$(whence -p git)
    self=$0
  else
    echo "Ve."
    return 1
  fi

  if [[ $# -eq 0 ]]; then
    if [[ -n "${BASH}" ]]; then
      type "${self}" | less
    elif [[ -n "${ZSH_NAME}" ]]; then
      which "${self}" | less
    else
      echo "Meh"
      return 1
    fi
  else
    op="$1"
    shift

    case "${op}" in
      (i) touch .gitignore; "${git}" init; "${self}" a.; "${self}" cim "$@" ;;
      (i!) "${self}" i "[supro] pravalorizu novdeponejon" ;;

      (s) "${git}" status ;;
      (c) "${git}" clone "$@" ;;
      (h) "${git}" show "$@" ;;
      (mv) "${git}" mv "$@" ;;
      (mv!) "${git}" mv -f "$@" ;;
      (me) "${git}" merge "$@" ;;
      (ta) "${git}" tag "$@" ;;
      (bl) "${git}" blame "$@" ;;

      (a) "${git}" add "$@" ;;
      (au) "${self}" a -u ;;
      (a.) "${self}" a . ;;
      (aum) "${self}" au; "${self}" cim "$@" ;;
      (a.m) "${self}" a.; "${self}" cim "$@" ;;
      (a.x) "${self}" a.m "x" ;;
      (aux) "${self}" aum "x" ;;
      (auxx) "${self}" aux; "${self}" rs 2 ;;
      (au.x) "${self}" a.x; "${self}" rs 2 ;;
      (auxx!) "${self}" auxx; "${self}" oo! ;;

      (cl) "${git}" clean "$@" ;;
      (cl!) "${self}" cl -f ;;

      (ci) "${git}" commit "$@" ;;
      (cia) "${self}" ci --amend "$@" ;;
      (cim) "${self}" ci --message "$@" ;;

      (co) "${git}" checkout "$@" ;;
      (com) "${self}" co main ;;
      (cot) "${self}" co trunk ;;
      (co!) "${self}" co --force "$@" ;;
      (cob) "${self}" co -b "$@" ;;

      (ls) "${git}" ls-files "$@" ;;
      (lsm) "${self}" ls -m ;;
      (lsd) "${self}" ls -d ;;
      (lsdrm) "${self}" lsd | xargs "${git}" rm ;;

      (rt) "${git}" reset "$@" ;;
      (rt!) "${self}" rt --hard "$@" ;;
      (rv) "${git}" revert "$@" ;;

      (g) "${git}" grep "$@" ;;
      (gi) "${self}" g -i "$@" ;;

      (f) "${git}" fetch "$@" ;;
      (fa) "${self}" f --all "$@" ;;

      (rm) "${git}" rm "$@" ;;
      (rmr) "${self}" rm -r "$@" ;;
      (rm!) "${self}" rm -rf "$@" ;;

      (rb) "${git}" rebase "$@" ;;
      (rbi) "${self}" rb --interactive "$@" ;;
      (rbc) "${self}" rb --continue "$@" ;;
      (rbs) "${self}" rb --skip "$@" ;;
      (rba) "${self}" rb --abort "$@" ;;
      (rbs) "${self}" rb --skip "$@" ;;
      (rbi!) "${self}" rbi --root "$@" ;;

      (ri) "${self}" rbi HEAD~"$1" ;;
      (rs) "${self}" rt --soft HEAD~"$1" && "${self}" cim "$(git log --format=%B --reverse HEAD..HEAD@{1} | head -1)" ;;

      (ph) "${git}" push "$@" ;;
      (phu) "${self}" ph -u "$@" ;;
      (ph!) "${self}" ph --force "$@" ;;
      (pho) "${self}" phu origin "$@" ;;
      (phoo) "${self}" phu origin "$(git brh)" ;;
      (phd) "${self}" ph --delete "$@" ;;
      (phdo) "${self}" phd origin "$(git brh)" ;;
      (oo) "${self}" ph origin "$(git brh)" ;;
      (oo!) "${self}" ph! origin "$(git brh)" ;;

      (pl) "${git}" pull "$@" ;;
      (pl!) "${self}" pl --force "$@" ;;
      (plr) "${self}" pl --rebase "$@" ;;
      (plro) "${self}" plr origin "$@" ;;
      (plroo) "${self}" plr origin "$(git brh)" ;;
      (plru) "${self}" plr upstream "$@" ;;
      (plruo) "${self}" plr upstream "$(git brh)" ;;

      (l) "${git}" log "$@" ;;
      (l1) "${self}" l -1 --pretty=%B ;;
      (lo) "${self}" l --oneline ;;
      (lp) "${self}" l --patch ;;
      (lp1) "${self}" lp -1 ;;
      (lpw) "${self}" lp --word-diff=color ;;

      (br) "${git}" branch "$@" ;;
      (bra) "${self}" br -a ;;
      (brm) "${self}" br -m "$@" ;;
      (brmh) "${self}" brm "$(git brh)" ;;
      (brd) "${self}" br -d "$@" ;;
      (brD) "${self}" br -D "$@" ;;
      (brh) "${git}" rev-parse --abbrev-ref HEAD ;;

      (d) "${git}" diff "$@" ;;
      (dc) "${git}" diff --cached "$@" ;;
      (dh) "${self}" d HEAD ;;
      (dhw) "${self}" d --word-diff=color ;;

      (re) "${git}" remote "$@" ;;
      (rea) "${self}" re add "$@" ;;
      (reao) "${self}" rea origin "$@" ;;
      (reau) "${self}" rea upstream "$@" ;;
      (rer) "${self}" re remove "$@" ;;
      (ren) "${self}" re rename "$@" ;;
      (rero) "${self}" rer origin "$@" ;;
      (reru) "${self}" rer upstream "$@" ;;
      (res) "${self}" re show "$@" ;;
      (reso) "${self}" res origin ;;
      (resu) "${self}" res upstream ;;

      (rl) "${git}" rev-list "$@" ;;
      (rla) "${self}" rl --all "$@" ;;
      (rl0) "${self}" rl --max-parents=0 HEAD ;;

      (cp) "${git}" cherry-pick "$@" ;;
      (cpc) "${self}" cp --continue "$@" ;;
      (cpa) "${self}" cp --abort "$@" ;;

      (fb) FILTER_BRANCH_SQUELCH_WARNING=1 "${git}" filter-branch "$@" ;;
      (fb!) "${self}" fb -f "$@" ;;
      (fbm) "${self}" fb! --msg-filter "$@" ;;
      (fbi) "${self}" fb! --index-filter "$@" ;;
      (fbe) "${self}" fb! --env-filter "$@" ;;

      (rp) "${git}" rev-parse "$@" ;;
      (rph) "${self}" rp HEAD ;;

      (st) "${git}" stash "$@" ;;
      (stp) "${self}" st pop "$@" ;;

      (subt) "${git}" subtree "$@" ;;
      (subta) "${self}" subt add "$@" ;;
      (subtph) "${self}" subt push "$@" ;;
      (subtpl) "${self}" subt pull "$@" ;;

      (subm) "${git}" submodule "$@" ;;
      (subms) "${self}" subm status "$@" ;;
      (submy) "${self}" subm summary "$@" ;;
      (submu) "${self}" subm update "$@" ;;
      (subma) "${self}" subm add "$@" ;;
      (submi) "${self}" subm init "$@" ;;

      (ref) "${git}" reflog "$@" ;;

      (de) "${git}" describe "$@" ;;
      (det) "${self}" de --tags "$@" ;;

      (*) "${git}" "${op}" "$@" ;;
    esac
  fi
}

function fp () {
  echo "${1:A}"
}

function d () {
  if (( ! $#@ )); then
      builtin cd
  elif [[ -d $argv[1] ]]; then
      builtin cd "$(fp $argv[1])"
      $argv[2,-1]
  elif [[ "$1" = -<-> ]]; then
      builtin cd $1 > /dev/null 2>&1
      $argv[2,-1]
  else
    echo "$0: no such file or directory: $1"
  fi
}

function dcl {
d ~/common-lisp "${@}"
}

function dvera {
dcl d vera "${@}"
}

function git-clone-loop {
for repo in /home/eldriv/Downloads/gitea-docker/gitea-data/git/repositories/valmiz/*.git; do
  repo_name=$(basename "${repo}".git)
  git clone "${repo}" "$repo_name"
done
}

function surl() {
  for repo in /home/eldriv/kreisystems/*; do
    if [ -d "$repo" ]; then
      repo_name=$(basename "$repo")
      echo "Changing URL for repo: $repo_name"
      git -C "${repo}" remote set-url origin "git@github.com:krei-systems/$repo_name.git"
    fi
  done
}


function crurl {
  for repo in /home/eldriv/kreisystems/*; do
      echo "=================================================="
      echo "DIR: ${repo}"
      git -C ${repo} remote -v   
      echo "=================================================="
  done
}
function sbcl-dev {
dev lisp sbcl "$@"
}


setopt SHARE_HISTORY
HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=999
setopt HIST_EXPIRE_DUPS_FIRST

function dps {
docker ps
}

function di {
docker images
}


# Check if the OS is Linux
linux_test() {
  [[ "$(uname)" == "Linux" ]]
}

linux_test && {
  function c {
    local sel=clipboard
    
    if [[ $# == 1 ]]; then
      if [[ -f "$1" ]]; then
        echo -n "$(fullpath $1)" | xclip -selection $sel
      else
        echo -n "${@}" | xclip -selection $sel
      fi
    else
      xclip -selection $sel "${@}"
    fi
  }

  function c! { xsel -ib < "$1" }
}

function c@ { echo "${@}" | c }
function c. { c "${PWD}" }
function c/ { c "${PWD}/$1" }


function mount-fns {
nix-shell -p nfs-utils --run "sudo mount.nfs -o vers=3,nolock 192.168.1.75:/nfs/eldriv ~/mycloud"
}

function umount-fns {
sudo umount ~/mycloud
}

