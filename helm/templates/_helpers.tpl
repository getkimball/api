{{/* vim: set filetype=mustache: */}}
{{/*
Expand the name of the chart.
*/}}
{{- define "kimball-api.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "kimball-api.fullname" -}}
{{- if .Values.fullnameOverride -}}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" -}}
{{- else -}}
{{- $name := default .Chart.Name .Values.nameOverride -}}
{{- if contains $name .Release.Name -}}
{{- .Release.Name | trunc 63 | trimSuffix "-" -}}
{{- else -}}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" -}}
{{- end -}}
{{- end -}}
{{- end -}}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "kimball-api.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Common labels
*/}}
{{- define "kimball-api.labels" -}}
helm.sh/chart: {{ include "kimball-api.chart" . }}
{{ include "kimball-api.genericSelectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end -}}

{{/*
Selector labels
*/}}
{{- define "kimball-api.genericSelectorLabels" -}}
app.kubernetes.io/component: api
{{- end -}}

{{- define "kimball-api.APISelectorLabels" -}}
{{ include "kimball-api.genericSelectorLabels" . }}
app.kubernetes.io/component: api
{{- end -}}

{{- define "kimball-api.DSSelectorLabels" -}}
{{ include "kimball-api.genericSelectorLabels" . }}
app.kubernetes.io/component: daemonset
{{- end -}}

{{/*
Create the name of the service account to use
*/}}
{{- define "kimball-api.serviceAccountName" -}}
{{- if .Values.serviceAccount.create -}}
    {{ default (include "kimball-api.fullname" .) .Values.serviceAccount.name }}
{{- else -}}
    {{ default "default" .Values.serviceAccount.name }}
{{- end -}}
{{- end -}}
