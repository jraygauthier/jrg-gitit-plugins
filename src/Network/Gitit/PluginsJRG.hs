module Network.Gitit.PluginsJRG (plugins) where

import Network.Gitit.Interface

import qualified Network.Gitit.Plugins.Ditaa as Ditaa
import qualified Network.Gitit.Plugins.Dot as Dot
import qualified Network.Gitit.Plugins.GnuPlot as GnuPlot
import qualified Network.Gitit.Plugins.HsDiagrams as HsDiagrams
import qualified Network.Gitit.Plugins.ImgTex as ImgTex
import qualified Network.Gitit.Plugins.IncludeAsCodeBlock as IncludeAsCodeBlock
--import qualified Network.Gitit.Plugins.IncludeDoc as IncludeDoc
import qualified Network.Gitit.Plugins.PlantUML as PlantUML
import qualified Network.Gitit.Plugins.ShowUser as ShowUser
--import qualified Network.Gitit.Plugins.Signature as Signature

plugins :: [Plugin]
plugins = [
  Ditaa.plugin,
  Dot.plugin,
  GnuPlot.plugin,
  HsDiagrams.plugin,
  ImgTex.plugin,
  IncludeAsCodeBlock.plugin,
  --IncludeDoc.plugin,
  PlantUML.plugin,
  ShowUser.plugin  {-,
  Signature.plugin-}]
