object PublishingServer: TPublishingServer
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  AllowPause = False
  DisplayName = 'Publishing server'
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
