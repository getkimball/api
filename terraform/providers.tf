provider "aws" {
  version = "~> 2.0"
  region  = "us-east-1"
}

provider "github" {
  token        = var.GITHUB_TOKEN
  organization = "getkimball"
}
