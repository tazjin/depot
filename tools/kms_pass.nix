# This tool mimics a subset of the interface of 'pass', but uses
# Google Cloud KMS for encryption.
#
# It is intended to be compatible with how 'kontemplate' invokes
# 'pass.'
#
# Only the 'show' and 'insert' commands are supported.

{ pkgs, kms, ... }:

let inherit (pkgs.third_party) google-cloud-sdk tree writeShellScriptBin;
in (writeShellScriptBin "pass" ''
  set -eo pipefail

  CMD="$1"
  readonly SECRET=$2
  readonly SECRET_PATH="$SECRETS_DIR/$SECRET"

  function secret_check {
    if [[ -z $SECRET ]]; then
      echo 'Secret must be specified'
      exit 1
    fi
  }

  if [[ -z $CMD ]]; then
    CMD="ls"
  fi

  case "$CMD" in
    ls)
       ${tree}/bin/tree $SECRETS_DIR
       ;;
    show)
      secret_check
      ${google-cloud-sdk}/bin/gcloud kms decrypt \
        --project ${kms.project} \
        --location ${kms.region} \
        --keyring ${kms.keyring} \
        --key ${kms.key} \
        --ciphertext-file $SECRET_PATH \
        --plaintext-file -
      ;;
    insert)
      secret_check
      ${google-cloud-sdk}/bin/gcloud kms encrypt \
        --project ${kms.project} \
        --location ${kms.region} \
        --keyring ${kms.keyring} \
        --key ${kms.key} \
        --ciphertext-file $SECRET_PATH \
        --plaintext-file -
      echo "Inserted secret '$SECRET'"
      ;;
    *)
      echo "Usage: pass show/insert <secret>"
      exit 1
      ;;
  esac
'') // { meta.enableCI = true; }
