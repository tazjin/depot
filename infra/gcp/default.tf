# Terraform configuration for the GCP project 'tazjins-infrastructure'

provider "google" {
  project = "tazjins-infrastructure"
  region  = "europe-north1"
}

# Configure a storage bucket in which to keep Terraform state and
# other data, such as Nixery's layers.
resource "google_storage_bucket" "tazjins-data" {
  name     = "tazjins-data"
  location = "EU"
}

terraform {
  backend "gcs" {
    bucket = "tazjins-data"
    prefix = "terraform"
  }
}

# Configure enabled APIs
resource "google_project_services" "primary" {
  project = "tazjins-infrastructure"
  services = [
    "bigquery-json.googleapis.com",
    "bigquerystorage.googleapis.com",
    "cloudapis.googleapis.com",
    "clouddebugger.googleapis.com",
    "cloudfunctions.googleapis.com",
    "cloudkms.googleapis.com",
    "cloudtrace.googleapis.com",
    "compute.googleapis.com",
    "container.googleapis.com",
    "containerregistry.googleapis.com",
    "datastore.googleapis.com",
    "dns.googleapis.com",
    "iam.googleapis.com",
    "iamcredentials.googleapis.com",
    "logging.googleapis.com",
    "monitoring.googleapis.com",
    "oslogin.googleapis.com",
    "pubsub.googleapis.com",
    "run.googleapis.com",
    "servicemanagement.googleapis.com",
    "serviceusage.googleapis.com",
    "sourcerepo.googleapis.com",
    "sql-component.googleapis.com",
    "storage-api.googleapis.com",
    "storage-component.googleapis.com",
  ]
}


# Configure the main Kubernetes cluster in which services are deployed
resource "google_container_cluster" "primary" {
  name     = "tazjin-cluster"
  location = "europe-north1"

  remove_default_node_pool = true
  initial_node_count       = 1
}

resource "google_container_node_pool" "primary_nodes" {
  name       = "primary-nodes"
  location   = "europe-north1"
  cluster    = google_container_cluster.primary.name
  node_count = 1

  node_config {
    preemptible  = true
    machine_type = "n1-standard-2"

    oauth_scopes = [
      "storage-rw",
      "logging-write",
      "monitoring",
      "https://www.googleapis.com/auth/source.read_only",
    ]
  }
}

# Configure a service account for which GCS URL signing keys can be created.
resource "google_service_account" "nixery" {
  account_id   = "nixery"
  display_name = "Nixery service account"
}

# Configure Cloud KMS for secret encryption
resource "google_kms_key_ring" "tazjins_keys" {
  name     = "tazjins-keys"
  location = "europe-north1"

  lifecycle {
    prevent_destroy = true
  }
}

resource "google_kms_crypto_key" "kontemplate_key" {
  name     = "kontemplate-key"
  key_ring = google_kms_key_ring.tazjins_keys.id

  lifecycle {
    prevent_destroy = true
  }
}

# Configure the git repository that contains everything.
resource "google_sourcerepo_repository" "depot" {
  name = "depot"
}
