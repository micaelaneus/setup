Remember that the ssh key used for setup must exist in Github.

```bash commands:
mkdir -p "${HOME}/.ssh"
ssh-keygen -t rsa -b 4096
pushd "${HOME}" && eval "$(curl -L https://raw.githubusercontent.com/micaelaneus/setup/master/init.sh)" && popd
```
